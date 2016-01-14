/**
 * Methods for working with SqlKit and Metadata simultaneously.
 */

import sqlkit.Select
import metadata.{Table, Metadata}
import util.Logging

/**
 * It might make sense to have two classes:
 *      class Soql(select: Select, md: Metadata) (we can probably come up with a better name)
 *      class SoqlPath
 * And Soql has a function called Soql.getColumnPath which returns instances of SoqlPath's.
 * Or something like that. The idea is that the combination of select + metadata is captured in one class.
 */
package object SoqlHelpers extends Logging {

  /**
   * Wrap a Select and augment it with additional information about the parts of the select
   * You should use this class as a "view" over a raw Select.  So instead of getting select projs,
   * you can get the normalized projs from this.
   *
   * SoqlWrapper is effectively a factory for getting things like NormalizedProjs so the logic of
   * how to construct NormalizedProj is here.
   *
   * The strategy for child relationship sub-selects (nested in a projection) is as follows:
   *   - Suppose we have a query with a child query such as
   *       SELECT a.Id, a.Name, (SELECT c.Id, c.Name from a.Contacts c)
   *       FROM Account a
   *   - Basically we *ignore* the bit that says "a.Contacts" and substitute "Contact" for it.
   *   - And then everything proceeds as normal (with regards to normalization and rendering)
   *   - The constructor arg `childTableName` is the name of the table that should be used for
   *     the child relationship
   * @param childTableName Some(tableName) if this is a child relationship query or
   *                       None, if this is an ordinary top-level query
   */
  class SoqlWrapper(md: Metadata, val select: Select, childTableName: Option[String] = None) {
    def getNormalizedSimpleProjs = {
      select.projs.filter(_.t.isSimple) map { proj =>
        val fullPath = expandAndCanonicalizePathString(proj.t.getSql)
        val normalizedPath = NormalizedProj(proj, fullPath)
        assert(normalizedPath.pathTableNames(0) == canonicalFromTableName)
        normalizedPath
      }
    }

    /**
     * See the scaladocs for the class to understand why we make this substition
     * @return unnormalized from table name
     */
    def fromTableName = {
      if (!childTableName.isDefined) {
        select.fromTableName
      } else {
        childTableName.get
      }
    }

    def fromTable: Table = {
      md.getTable(fromTableName).get
    }

    /**
     * @return a Seq of (childRelationshipName, soqlWrapper) tuples for each child relationship
     *         sub-select in this query, with the sub-Select wrapped by a SoqlWrapper
     *         Note that the childRelationshipName is used to extract the child rows from JSON
     */
    def getWrappedSelectProjs: Seq[(String, SoqlWrapper)] = {
      select.getSelectProjs map { selectProj =>
        val childSelect = selectProj.select
        val childFrom = childSelect.fromTableName
        var childRelationshipName =
          if (childFrom.contains('.')) {
            childFrom.split('.')(1)
          } else {
            childFrom
          }
        val childRelationship = fromTable.getChildRelationship(childRelationshipName).get
        childRelationshipName = childRelationship.name
        val childTable = childRelationship.childTable
        (childRelationshipName, new SoqlWrapper(md, childSelect, Some(childTable.name)))
      }
    }

    private[this] def canonicalFromTableName = md.canonicalTableName(fromTableName).get

    private[this] def expandAndCanonicalizePathString(origPathString: String) = {
      val rootAliasOverride = {
        // For child sub-selects only, override the alias for the FROM table
        // to refer directly to the actual table name, rather than the *child relationship*
        // See the example in the scaladoc for the class
        val fromAliasOpt = select.from.alias
        if (childTableName.isDefined && fromAliasOpt.isDefined) {
          Map(fromAliasOpt.get -> childTableName.get)
        } else {
          // If there is no alias, don't worry, path expressions can't refer to anything bad
          Map[String, String]()
        }
      }
      val expandedPathString = select.expandAliases(origPathString, rootAliasOverride)
      val expandedPath = expandedPathString.split("""\.""")

      // Find all the lookups in the path
      val lookups = {
        val tmpLookups = expandedPath.dropRight(1).toList
        if (tmpLookups.isEmpty) List(fromTableName)
        // prepend the from table, if it wasn't initially present
        else if (tmpLookups(0).toLowerCase != fromTableName.toLowerCase) fromTableName::tmpLookups
        else tmpLookups
      }
      val finalColumn = expandedPath.last

      // Finally, normalize the lookup names to the metadata capitalization
      canonicalizePath(lookups :+ finalColumn, lastLookupIsColumn = true)
    }


    /**
     * Given a path, like ["contact", "owner", "firstname"],
     * canonicalize it: ["Contact", "Owner", "FirstName"].
     */
    private[this] def canonicalizePath(origLookups: List[String], lastLookupIsColumn: Boolean): Seq[String] = {
      val tableLookups = {
        if (lastLookupIsColumn)
          origLookups.dropRight(1)
        else
          origLookups
      }

      try {
        var currentTable: Table = md.getTable(tableLookups.head).get
        var normalizedLookups = new collection.mutable.ListBuffer[String]
        normalizedLookups += currentTable.name
        for (lookup <- tableLookups.tail) {
          val fkInfo = currentTable.getForeignKeyInfo(lookup).get
          val newPathTable = fkInfo.sfdcLookupRelationshipName.get
          normalizedLookups += newPathTable
          currentTable = fkInfo.targetTable
        }
        if (lastLookupIsColumn)
          normalizedLookups += currentTable.getColumn(origLookups.last).get.name

        normalizedLookups
      } catch {
        case e: Exception =>
          logger.error("Canonicalizing path " + origLookups + " failed", e)
          throw e
      }
    }
  }

  /**
   * Wrap a Proj and augment it with additional information
   * @todo add a member "rootAliasedPath" which returns a String using the root alias
   * @param origProj the proj being wrapped
   * @param fullPath the fully normalized (expanded and canonicalized) path, leading with the root table name
   *                 and ending with the final column name
   */
  case class NormalizedProj(origProj: sqlkit.Aliased[sqlkit.Proj], private val fullPath: Seq[String]) {
    def pathTableNames = fullPath.dropRight(1)
    def finalColumnName = fullPath.last
    def isRootColumn = pathTableNames.size == 1
  }
}

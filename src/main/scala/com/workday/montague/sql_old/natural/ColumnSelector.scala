package natural

import metadata.{Table, Metadata}


/**
 * Interface that tells the (logical) SQL generator what columns to add to the SELECT clause
 * Suppose we have a query like
 *
 *    SELECT
 *    FROM Account a, a.Contacts c, a.Owner o, a.Owner.Role or
 *
 * Then the SQL generator will request columns for the following:
 *
 *    getColumnsFor(Account, "")
 *    getColumnsFor(Contact, t.Contacts)
 *    getColumnsFor(User, t.Owner)
 *    getColumnsFor(UserRole, t.Owner.Role)
 */
trait ColumnSelector {
  /**
   * @param tableName What table we want columns for
   * @param pathFromRootTable TBD.  Will never be null.  Empty string ("") indicates this is the root table.
   *                          Otherwise, a path (might start with the root table's alias or some generic alias like 't').
   *                          The path indicates whether this is a child of the root, or a lookup, or a lookup off of
   *                          a child, etc.
   * @return names of columns of tableName that should be added to the SELECT clause
   */
  def getDisplayColumnsFor(tableName: String, pathFromRootTable: String): Seq[String]

  def getSqlColumnsFor(tableName: String, pathFromRootTable: String): Seq[String]
}

/**
 * A trait that implements ColumnSelector.getSqlColumnsFor() by using
 * getDisplayColumnsFor() and always adding the primary key.
 *
 * The primary key is necessary for hyper-linking the result sets to the underlying
 * records.
 */
trait SelectPrimaryKeyForSql extends ColumnSelector {
  def metadata: Metadata

  def getSqlColumnsFor(tableName: String, pathFromRootTable: String) = {
    val table = metadata.getTable(tableName).get
    val primaryKeyColumnName = table.primaryKey.name
    (Seq(primaryKeyColumnName) ++ getDisplayColumnsFor(tableName, "")).distinct
  }
}

/**
 * Layouts specify what columns to display and how to display them on the page
 * In theory, one can choose among different types of layouts, for example, possibly:
 * - Automatic/default layouts when the user chooses nothing
 * - Page layouts defined in Salesforce
 * - Search layouts defined in Salesforce
 * - Named column sets saved in Upshot
 * - Ad-hoc columns chosen using the column picker
 *
 * Today, we only have an implementation of the very last, and "Automatic" is represented by None
 */
trait Layout

/**
 * A layout where the columns have been explicitly selected by the user
 * This layout is special because:
 *    - It's created using the column picker widget
 *    - Every other Layout needs to supply a representation as a ColumnLayout
 *      so that we can support the behavior that when the user switches from another
 *      layout to this layout, then the column picker widget is pre-populated with
 *      the columns effectively chosen by the other layout type.  This is called the "effective layout".
 *
 * @todo Handle child tables and lookups
 */
case class ColumnLayout(tableName: String, columnNames: Seq[String]) extends Layout

class UserSelectedColumnsSelector(val metadata: Metadata,
                                  userSelectedColumns: Option[ColumnLayout], default: ColumnSelector)
  extends ColumnSelector with SelectPrimaryKeyForSql {
  /**
   * This allows us to represent the state where the hasn't made any explicit column choices,
   * and carry the state across requests.
   * In the future, we might want to expand this to cover things like where the user can choose named column sets,
   * or page layouts, etc.
   */
  def getExplicitlySelectedColumns(tableName: String) = {
    userSelectedColumns.map { usc =>
      if (usc.tableName == tableName) {
        usc.columnNames
      } else {
        Seq()
      }
    } getOrElse {
      Seq()
    }
  }

  /**
   * This allows the Customize Columns UI to pre-select the columns that are effectively selected by whatever
   * explicit selection has been made.  In particular, the empty selection by the user implies the default column
   * selection.
   */
  def getEffectiveSelectedColumns(tableName: String) = {
    getDisplayColumnsFor(tableName, "")
  }

  override def getDisplayColumnsFor(tableName: String, pathFromRoot: String) = {
    if (userSelectedColumns.isDefined && userSelectedColumns.get.tableName == tableName && !userSelectedColumns.get.columnNames.isEmpty) {
      userSelectedColumns.get.columnNames
    } else {
      default.getDisplayColumnsFor(tableName, pathFromRoot)
    }
  }
}

/**
 * Return Nil, which indicates to SqlGen to use *
 */
object StarColumnsSelector extends ColumnSelector {
  override def getDisplayColumnsFor(tableName: String, pathFromRootTable: String) = {
    Nil
  }

  override def getSqlColumnsFor(tableName: String, pathFromRootTable: String) = {
    Nil
  }
}

/**
 * For any requested table, just choose all the columns from that table
 */
class AllColumnsSelector(md: Metadata) extends ColumnSelector {
  override def getDisplayColumnsFor(tableName: String, pathFromRootTable: String) = {
    md.getTable(tableName).map(_.columns.filter(_.hasDisplayLabel).map(_.name)).getOrElse(Nil)
  }

  override def getSqlColumnsFor(tableName: String, pathFromRootTable: String) = getDisplayColumnsFor(tableName, pathFromRootTable)
}

class SfdcDefaultColumnsSelector(override val metadata: Metadata, useSmallLayout: Boolean = false) extends ColumnSelector with SelectPrimaryKeyForSql {

  // @todo add support for foreign keys here
  override def getDisplayColumnsFor(tableName: String, pathFromRootTable: String) = {
    if (!useSmallLayout) {
      tableName match {
        case "Account" => "Name" :: "Industry" :: "Phone" :: "BillingState" :: Nil
        case "Contact" => "Name" :: "Email" :: "Phone" :: Nil
        case "Opportunity" => "Name" :: "Amount" :: "StageName" :: "CloseDate" :: Nil
        case "User" => "Name" :: Nil
        case "Case" => "CaseNumber" :: "Subject" :: "Status" :: "Priority" :: Nil
        case "Task" => "Subject" :: "ActivityDate" :: "Status" :: "Priority" :: "Description" :: Nil // @todo we really need to add "What.Name" and "Who.Name" here and in the other activity cases
        case "Event" => "Subject" :: "Location" :: "StartDateTime" :: "EndDateTime" :: "Description" :: Nil
        case _ => {
          val MAX_COLUMNS = 5
          val table = metadata.getTable(tableName).get
          assert(table.columns.filter(_.isNameField).length <= 1)
          val nameColumns = table.columns.find(_.isNameField).map(_.name).getOrElse(table.primaryKey.name) :: Nil
          (nameColumns ++ table.columns.filter(c => !c.isNameField && !c.isPrimaryKey && c.hasDisplayLabel).map(_.name)).take(MAX_COLUMNS)
        }
      }
    } else {
      tableName match {
        case "Account" => "Name" :: Nil
        case "Contact" => "Name" :: "Phone" :: Nil
        case "Opportunity" => "Name" :: Nil
        case "User" => "Name" :: Nil
        case "Case" => "CaseNumber" :: "Subject" :: "Status" :: Nil
        case "Task" => "Subject" :: "ActivityDate" :: "Status" :: Nil
        case "Event" => "Subject" :: "Location" :: "StartDateTime" :: "EndDateTime" :: Nil
        case _ =>
          val table = metadata.getTable(tableName).get
          table.columns.find(_.isNameField).map(_.name).getOrElse(table.primaryKey.name) :: Nil
      }
    }
  }
}

class SfdcFixedColumnSelector(fixedTableName: String, fixedPathFromRootTable: String, fixedColumns: Seq[String]) extends ColumnSelector {
  def getDisplayColumnsFor(tableName: String, pathFromRootTable: String): Seq[String] = {
    fixedColumns      // Workaround because tableName and pathFromRootTable are not actually correct.
    /*
    if (tableName == fixedTableName && pathFromRootTable == fixedPathFromRootTable) fixedColumns
    else Nil
    */
  }

  override def getSqlColumnsFor(tableName: String, pathFromRootTable: String): Seq[String] = getDisplayColumnsFor(tableName, pathFromRootTable)
}

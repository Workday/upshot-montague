package sfdc

import metadata._
import sqlkit._
import sqlkit.Select
import sqlkit.BaseRelation
import play.api.templates.HtmlFormat
import scala.Some
import play.api.libs.json._
import play.api.libs.json.{JsString, JsNumber, JsObject}
import scala.collection.immutable.ListMap
import natural.{ColumnSelector, SfdcFixedColumnSelector}
import render._
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTimeZone, DateTime}

/**
 * Information needed for proper rendering
 *
 * @param sfdcInstanceUrl used to generate hyperlinks to the Salesforce record
 * @param linkNames whether or not to generate hyperlinks to the Salesforce record
 * @param timeZone what time zone we should localize to when rendering date time's
 */
case class RenderOptions(sfdcInstanceUrl: String, linkNames: Boolean = true, timeZone: DateTimeZone)

/**
 * @param withJudgedSoql We have a separate render factory for Select statements that are from analyst (judged) SOQL.
 */
class SfdcRenderFactory(md: Metadata, selectedColumns: ColumnSelector, withJudgedSoql: Boolean,
                        renderOptions: RenderOptions) extends RowRenderFactory {

  def createRenderForSelect(select: Select): TreeRowRender = {
    if (select.isAggregateQuery) {
      // The column names from aggregate queries don't correspond to known column names in the metadata, so revert to the "dumb" behavior
      new SfdcSimpleMapRender(select.projs.toList)
    } else {
      if (withJudgedSoql)
        createJudgedSoqlRowRender(select)
      else
        createParsedSoqlRowRender(select)
    }
  }

  /**
   * Creates a row render for a non-aggregate SOQL query. This is only for Parsed SOQL selects, which are actually SQL-like before SQLToSoql.
   *
   * For example:
   *
   * SELECT a.name, u1.Name, u2.Name, (SELECT c.name FROM a.Contacts c), (SELECT o.name from a.Opportunities o)
   * FROM account a, a.Owner u1, a.CreatedBy u2
   *
   *
   * This query can contain:
   * - A single "main" query, that is, the projections against the FROM object, in this case account a
   * - Multiple lookup joins, in this case u1 and u2 and their projections
   * - Multiple child joins, in this case c and o, which may be inner or outer joins (we don't care here)
   */
  private[this] def createParsedSoqlRowRender(select: Select) = {
    val table = md.getTable(select.from.t.asInstanceOf[BaseRelation].from).get

    // Create the row render for the main object
    val expectedColumns = getProjColumnNamesFromAlias(select, select.from.alias.get).toList
    val mainRowRender = new SfdcRowRender(table, expectedColumns, selectedColumns, renderOptions)

    /**
     * @todo Rewrite using the code paths from createJudgedSoqlRowRender, particularly for lookup joins.
     * That code can handle nested lookups, and also it's better not to duplicate functionality.
     * In particular, remove all uses of getProjColumnNamesFromAlias.
     */

    // Create the row renders for all lookup joins
    val soqltransformer = new SqlToSoqlGenerator(md)
    val allJoinInfos = soqltransformer.generateAllJoinInfos(select)
    val lookupJoinRowRenders = ListMap(allJoinInfos.collect({ case j: SfdcLookupJoin => j}).map { lookupJoin =>
      val expectedColumns = getProjColumnNamesFromAlias(select, lookupJoin.alias).toList
      (table.name + "." + lookupJoin.foreignKeyInfo.sfdcLookupRelationshipName.get,
        new SfdcRowRender(lookupJoin.foreignKeyInfo.targetTable, expectedColumns, selectedColumns, renderOptions))
    }: _*)

    // Create the row render for at most 1 child join
    // @todo: support more than 1 child join (multi-block)
    val childJoinRowRender = allJoinInfos.collect({ case j: SfdcChildJoin => j }).headOption map { childJoin =>
      val expectedColumns = getProjColumnNamesFromAlias(childJoin.childJoinSelect, childJoin.childJoinSelect.from.alias.get).toList
      (childJoin.childRelationship.name,
        // @todo generate and handle lookups in child sub-selects (should happen when we consolidate the judged
        // and parsed code paths
        new SoqlJoinRowRenderChain(
          new SfdcRowRender(childJoin.childRelationship.childTable, expectedColumns, selectedColumns, renderOptions),
          ListMap(), Map()))
    }

    // Create the composite row render that is made up of all the row renders we created above
    new SoqlJoinRowRenderChain(mainRowRender, lookupJoinRowRenders, childJoinRowRender.toMap)
  }

  private[this] def createJudgedSoqlRowRender(select: Select) = {
    val wrapper = new SoqlHelpers.SoqlWrapper(md, select)
    val (mainRowRender, lookupJoinRowRenders) = getRowRender(wrapper)

    val childRowRenders = wrapper.getWrappedSelectProjs map {
      case (childRelationshipName, childSelectWrapper) =>
        // The childRelationshipName is used to extract the child row results from the JSON
        val (childRowMainRender, childRowLookupRenders) = getRowRender(childSelectWrapper)
        // Note that we support lookups for these child sub-selects, and that Salesforce doesn't (today) allow
        // child sub-selects within child sub-selects, though we can theoretically handle them here
        val childRowRender = new SoqlJoinRowRenderChain(childRowMainRender, ListMap(childRowLookupRenders.toSeq: _*), Map())
        (childRelationshipName, childRowRender)
    }

    // Create the composite row render that is made up of all the row renders we created above
    new SoqlJoinRowRenderChain(mainRowRender, ListMap(lookupJoinRowRenders.toList: _*),
      ListMap(childRowRenders: _*))
  }

  private[this] def getRowRender(wrapper: SoqlHelpers.SoqlWrapper) = {
    val select = wrapper.select
    val table = wrapper.fromTable
    val tableName = table.name

    assert(select.sqlJoins.isEmpty)   // In judged Soql, we will only have SoqlJoins

    val canonicalProjs = wrapper.getNormalizedSimpleProjs

    // Create one row render for each path
    val uniquePaths = canonicalProjs.map(_.pathTableNames).toSet

    // Map from a unique path to a list of columns
    val uniquePathsToColumns = uniquePaths.map(path =>
      (path -> canonicalProjs.filter(proj => (path == proj.pathTableNames)).map(_.finalColumnName).toList)).toMap

    val uniquePathsToRowRenders = uniquePathsToColumns.map(x => {
      val (path, sqlSelectedColumns) = x
      var pathTable: Table = md.getTable(path.head).get
      var headerPrefix: String = ""
      for (lookup <- path.tail) {
        headerPrefix += pathTable.getForeignKeyInfo(lookup).get.displayLabel + " "
        pathTable = pathTable.getLookupRelationship(lookup).get
      }

      // For the time being, we override the selectedColumns with those in the judged SOQL.
      val columnsToDisplay = filterJudgedColumnsToDisplay(pathTable, sqlSelectedColumns)
      val fixedSelectedColumns = new SfdcFixedColumnSelector(tableName, path.mkString("."), columnsToDisplay)
      (path.mkString("."), new SfdcRowRender(pathTable, sqlSelectedColumns, fixedSelectedColumns, renderOptions, headerPrefix))
    })

    val (mainRowRender, lookupJoinRowRenders) =
      if (uniquePathsToRowRenders.contains(tableName)) {
        (uniquePathsToRowRenders.get(tableName).get, uniquePathsToRowRenders - tableName)
      } else {
        (new SfdcRowRender(table, Nil, selectedColumns, renderOptions), uniquePathsToRowRenders)
      }

    (mainRowRender, lookupJoinRowRenders)
  }

  /**
   * Given some columns that were chosen by analyst judgment (these are given as
   * `sqlSelectedColumns`, since it's the SQL that tells us what columns were chosen),
   * pick out the ones that should be rendered for display.
   */
  private[this] def filterJudgedColumnsToDisplay(table: Table, sqlSelectedColumns: Seq[String]): Seq[String] = {
    sqlSelectedColumns filter { sqlSelectedColumn =>
      table.getColumn(sqlSelectedColumn) map { column =>
        !column.isPrimaryKey
      } getOrElse {
        true
      }
    }
  }

  /**
   * Given a Select and a tableAlias, get the column names from all projections from the table with that alias
   *
   * So if you have "SELECT a.name, a.industry, a.ownerid" and tableAlias "a", we return "name", "industry",
   * "ownerid"
   * @todo: Removeme, and use methods in createJudgedSoqlRowRender instead.
   */
  private[this] def getProjColumnNamesFromAlias(select: Select, alias: String) = {
    SqlToSoqlGenerator.getProjsByTableAlias(select.projs, alias) map { proj =>
      proj.alias.getOrElse {
        proj.t.getSqlColumn
      }
    }
  }
}

/**
 * Renders a query that contains lookup or child joins.  We use multiple RowRender instances to handle the main select
 * and the joins separately, and concatenate the results.
 *
 * @param childJoinRowRenders Note that these are themselves SoqlJoinRowRenderChain's.  Child joins can in theory be
 *                            recursively defined (i.e. they can have child joins themselves).  In practice, they
 *                            only have lookups and no child joins, but this implementation is simpler and allows us
 *                            to handle future changes.
 */
class SoqlJoinRowRenderChain(mainRowRender: SfdcRowRender, lookupJoinRowRenders: ListMap[String, SfdcRowRender],
                             childJoinRowRenders: Map[String, SoqlJoinRowRenderChain]) extends TreeRowRender {
  /// How to render the header (columns)
  def renderHeader: TreeHeader = {
    TreeHeader("", mainRowRender.renderHeader ++ lookupJoinRowRenders.flatMap(_._2.renderHeader),
      childJoinRowRenders.map(child =>
        // We flatten the child header here since the child render header is
        // a TreeHeader but we only expect to see main column headers,
        // and not further TreeHeaders
        TreeChildHeader(child._1, child._2.renderHeader.columns)).toSeq)
  }

  def renderRow(row: Map[String, Any]): TreeRow = {
    // Generate the rendering for the main row
    val mainRow = mainRowRender.renderRow(row)
    val mainRowUrl = mainRowRender.getRowLinkUrl(row)

    // Generate the rendering for all lookup objects
    val lookupSubRow = lookupJoinRowRenders flatMap { lookupJoinRowRender =>
      val (lookupName, lookupRowRender) = lookupJoinRowRender
//      val insensitiveRow = ListMap[String, Any](row.toList.map(kv => (kv._1.toLowerCase, kv._2)): _*)
      val lookupPath = lookupName.split("""\.""").toList
      assert(lookupPath(0) == mainRowRender.table.name)
      var lookupValueOpt2: Option[Any] = Some(row)
      for(p <- lookupPath.tail) {
        if (lookupValueOpt2.isDefined)
          lookupValueOpt2 = lookupValueOpt2.get.asInstanceOf[Map[String, Any]].get(p)
      }

      val lookupValueOpt = lookupValueOpt2
      lookupValueOpt flatMap { lookupValue =>
        if (lookupValue.isInstanceOf[Map[_, _]])
          Some(lookupRowRender.renderRow(lookupValue.asInstanceOf[Map[String, Any]]))
        else
          None
      } getOrElse {
        // The lookupValue was None
        Seq.fill(lookupRowRender.numColumns)("-")
      }
    }

    // Generate the rendering for the child join
    val childSubRows = childJoinRowRenders flatMap { childJoinRowRender =>
      val childName = childJoinRowRender._1
      val childRowRender = childJoinRowRender._2
      val childValueOpt = row.get(childName)
      childValueOpt flatMap { childValue =>
        if (childValue != null) {
          val results = SfdcRowRender.jsonToResults(childValue.asInstanceOf[JsObject])
          val rows = results map {
                row => childRowRender.renderRow(row)
              }
          if (!rows.isEmpty) {
            Some(childJoinRowRender._1 -> rows)
          } else {
            None
          }
        } else {
          // There was no child sub-document in the JSON
          None
        }
      }
    }

    TreeRow("", mainRowUrl, mainRow ++ lookupSubRow,
      childSubRows.map(entry =>
        // Note that we flatten the child row rendering here
        // since the child render result is a TreeRow, but we only
        // expect to see main columnValues, not further child
        // rows
        (entry._1, entry._2.map(_.columnValues))))
  }

}

/**
 * Special implementation of RowRender for Salesforce objects that does Salesforce-specific things like
 * map names to hyperlinks pointing to Salesforce detail pages.
 * @param expectedColumns The columns we expect to see in the resultset.  The old scheme (e.g. in SimpleRender)
 *                        relied on grabbing the column headers from the first row of the JDBC resultset.  This
 *                        doesn't work for SOQL child joins because the first row in the resultset may have no
 *                        child rows.
 */
class SfdcRowRender(val table: Table, expectedColumns: List[String], selectedColumns: ColumnSelector,
                    renderOptions: RenderOptions, headerPrefix: String = "") {
  val displayColumns = selectedColumns.getDisplayColumnsFor(table.name, "").toList

  private[this] lazy val resultDisplayColumnNames = displayColumns.filter(columnName => expectedColumns.contains(columnName) && table.containsColumn(columnName))

  private[this] lazy val resultDisplayColumns = resultDisplayColumnNames.map(table.getColumn(_).get)

  def numColumns = resultDisplayColumnNames.size

  // @todo: Add a space
  def renderHeader: List[String] = resultDisplayColumns.map(headerPrefix + _.displayLabel)

  /**
   * @return the URL to the Salesforce detail page for this row
   */
  def getRowLinkUrl(row: Map[String, Any]) = {
    val id = row.get(table.primaryKey.name)
    if (id.isDefined) {
      Some(s"${renderOptions.sfdcInstanceUrl}/${id.get}")
    } else {
      None
    }
  }

  def renderRow(row: Map[String, Any]): Seq[String] = {
    resultDisplayColumns map { displayColumn =>
      renderCell(row, displayColumn)
    }
  }

  private[this] def renderCell(row: Map[String, Any], column: Column) = {
    val value = row.get(column.name)
    var result = renderDefault(column, value)
    if (column.isNameField && renderOptions.linkNames) {
      getRowLinkUrl(row) foreach { linkUrl =>
        result = s"<a href='${linkUrl}'>${result}</a>"
      }
    }
    result
  }

  private[this] def renderDefault(column: Column, cellValue: Option[Any]) = {
    val renderedStr =
      cellValue match {
        case Some(null) => ""
        case Some(value) =>
          if (column.isDateOnly && value.isInstanceOf[DateTime]) {
            // If the column is a Date-only, but we have a DateTime value, then cast it down to a Date-only
            // in a time-zone-respecting way
            // Really, we're talking about CampaignMember.CreatedDate => "Member First Associated Date" here
            var dateTime = value.asInstanceOf[DateTime]
            dateTime = dateTime.toDateTime(renderOptions.timeZone)
            dateTime.toLocalDate.toString
          } else {
            value.toString
          }
        case None => ""
      }
    HtmlFormat.escape(renderedStr).toString
  }

  override def toString = {
    s"SfdcRowRender(table = ${table.name}, sfdcInstanceUrl = ${renderOptions.sfdcInstanceUrl}, expectedColumns = $expectedColumns, selectedColumns = $selectedColumns, headerPrefix = $headerPrefix)"
  }
}

object SfdcRowRender {

  /**
   * Convert the JSON response from SFDC into a list of rows.  This can be used recursively for child joins, where
   * the results of the child join are embedded as a JSON value for a column.
   * @todo consider changing this to a case class so it's not such a pain to work with
   *       possibly even use the TreeRow class, although then we lose the Mappiness
   */
  def jsonToResults(json: JsValue): Iterable[Map[String, Any]] = {
    val records = (json \ "records").as[List[Map[String, JsValue]]]
    records.map(jsonToRecord(_))
  }

  // Why are we passing in a Map here? This should either be a JsValue, or when the Map was created it should
  // recursively create sub-Maps, i.e. there should be no Map[String, JsValue]
  def jsonToRecord(record: Map[String, JsValue]): Map[String, Any] = {
    //val record = obj.as[Map[String, JsValue]]
    record.filter(_._1 != "attributes") flatMap { _ match {
        case (key, JsString(str)) =>
          val maybeDateTime = parseSalesforceRestApiDateTimeStr(str)
          maybeDateTime map { (key, _) } orElse { Some(key, str) }
        case (key, JsNumber(bigdecimal)) => Some(key, bigdecimal)
        case (key, JsNull) => Some(key, null)
        // Lookup relationships, e.g.
        //  select id, account.owner.name, account.owner.firstname from account
        // return values of the form:
        //   {"attributes": ...,"Name":"Joseph T", "FirstName": "Joseph"}
        case (key, obj: JsObject) if obj.keys.contains("attributes") => {
          // I'm not sure why I have to do this cast
          val r = jsonToRecord(obj.value.asInstanceOf[scala.collection.immutable.Map[String,play.api.libs.json.JsValue]])
          Some(key, r)
          //r map {case (subkey, value) => (s"$key.$subkey", value) }
        }
        // TODO: we should try to detect dates since JSON has no native Date type
        case (key, obj: JsObject) => Some(key, obj)
        case (key, x) => Some(key, x.toString)
      }
    }
  }

  private[this] def parseSalesforceRestApiDateTimeStr(str: String) = {
    val formatter = ISODateTimeFormat.dateTimeParser
    try {
      // Check that this contains both a date and a time, the 'T' indicates that there is a time element here
      if (str.contains("T")) {
        Some(formatter.parseDateTime(str))
      } else {
        None
      }
    } catch {
      case _: IllegalArgumentException => None
    }
  }
}

/**
 * This class should only be used for simple aggregate queries.  It doesn't handle lookups or
 * anything more complex than simple column names.
 */
class SfdcSimpleMapRender(private val projs: List[Aliased[Proj]]) extends TreeRowRender {
  def columnNames = {
    var exprCnt = 0
    projs map { proj =>
      proj.alias.getOrElse {
        if (proj.t.isAggregate) {
          exprCnt += 1
          s"expr${exprCnt-1}"
        } else if (proj.t.isSimple) {
          proj.t.getSqlColumn
        } else {
          throw new Exception(s"Projection is neither aggregate nor simple: ${proj.t}")
        }
      }
    }
  }

  /// How to render the header (columns)
  def renderHeader = new TreeHeader(columnNames)

  def renderRow(row: Map[String, Any]): TreeRow = {
    new TreeRow(columnNames.map(columnName => renderDefault(row.get(columnName))))
  }

  private[this] def renderDefault(cellValue: Option[Any]) = {
    cellValue match {
      case Some(null) => ""
      case Some(value) => HtmlFormat.escape(value.toString).toString
      case None => ""
    }
  }
}

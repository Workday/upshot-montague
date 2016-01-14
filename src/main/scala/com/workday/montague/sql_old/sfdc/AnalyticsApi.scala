package sfdc

import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

// The implicit definitions have to be at the top
object Report {
  implicit val jsReads = Json.reads[Report]
}

object ReportResults {
  implicit val aciReads = Json.reads[AggregateColumnInfo]
  implicit val dciReads = Json.reads[DetailColumnInfo]
  implicit val gciReads = Json.reads[GroupingColumnInfo]
  implicit val extendedMetadataReads = Json.reads[ReportExtendedMetadata]
  implicit val groupingReads = Json.reads[Grouping]
  implicit val groupingValueReads = Json.reads[GroupingValue]
  implicit val aggReads = Json.reads[Aggregate]
  implicit val cellReads = Json.reads[CellValue]
  implicit val factsReads = Json.reads[Facts]
  implicit val attributesReads = Json.reads[ReportAttributes]
  implicit val rtReads = Json.reads[ReportType]
  implicit val rfReads = Json.reads[ReportFilter]
  implicit val rmReads = Json.reads[ReportMetadata]
  implicit val jsReads = Json.reads[ReportResults]
}

/**
 * Wrapper around the JSON returned by the Analytics REST API to
 * get more intelligently structured results
 *
 * See http://www.salesforce.com/us/developer/docs/api_analytics/index.htm
 * for docs on the Salesforce Analytics API
 */
class AnalyticsApi(restApi: RestApi) {

  def getRecentReports = {
    restApi.analyticsReports.map(_.as[Seq[Report]])
  }

  def executeReport(reportId: String, includeDetails: Boolean) = {
    restApi.executeReport(reportId, includeDetails).map(_.as[ReportResults])
  }

  def executeReportSfdcJson(reportId: String, includeDetails: Boolean) = {
    restApi.executeReport(reportId, includeDetails)
  }

}

/**
 * The following case classes are derived verbatim from the JSON examples
 * We use them to serialize so member names must match exactly the JSON names
 */
case class Report(describeUrl: String, id: String, instancesUrl: String, name: String, url: String)

case class ReportResults(attributes: ReportAttributes,
                         allData: Boolean,
                         factMap: Map[String, Facts],
                         /** key is always "groupings" */
                         groupingsAcross: Map[String, Seq[GroupingValue]],
                         groupingsDown: Map[String, Seq[GroupingValue]],
                         hasDetailRows: Boolean,
                         reportExtendedMetadata: ReportExtendedMetadata,
                         reportMetadata: ReportMetadata) {
  def groupingValuesAcross = groupingsAcross("groupings")
  def groupingValuesDown = groupingsDown("groupings")
}
case class ReportAttributes(describeUrl: String, instancesUrl: String, reportId: String, reportName: String/*, _type: String */)
case class Facts(aggregates: Seq[Aggregate],
                 /** key in map is always "dataCells" */
                 rows: Seq[Map[String, Seq[CellValue]]]) {
  def rowCells = rows.map(_.apply("dataCells"))
}
case class CellValue(label: String, value: JsValue)
case class Aggregate(label: String, value: BigDecimal)
case class GroupingValue(groupings: Seq[GroupingValue], key: String, label: String, value: Option[String])
case class ReportExtendedMetadata(aggregateColumnInfo: Map[String, AggregateColumnInfo],
  detailColumnInfo: Map[String, DetailColumnInfo],
  groupingColumnInfo: Map[String, GroupingColumnInfo])
case class AggregateColumnInfo(acrossGroupingContext: Option[String], dataType: String, downGroupingContext: Option[String], label: String)
case class DetailColumnInfo(dataType: String, label: String)
case class GroupingColumnInfo(dataType: String, groupingLevel: Int, label: String)

case class ReportMetadata(
  aggregates: Seq[String],
  currency: Option[String],
  detailColumns: Seq[String],
  developerName: String,
  groupingsAcross: Seq[Grouping],
  groupingsDown: Seq[Grouping],
  id: String,
  name: String,
  reportBooleanFilter: Option[String],
  reportFilters: Seq[ReportFilter],
  reportFormat: String,
  reportType: ReportType
)

case class Grouping(dateGranularity: String, name: String, sortOrder: String)
case class ReportFilter(column: String, operator: String, value: String)
case class ReportType(label: String/*, _type: String*/)

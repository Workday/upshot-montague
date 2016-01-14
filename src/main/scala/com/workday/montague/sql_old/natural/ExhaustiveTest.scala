/**
 * Not a formal test suite.
 * This just runs many queries to show the output.
 **/

package natural

import com.novus.salat._
import sfdc.{SfdcMetadata, SqlToSoqlGenerator, SfdcNQMetadata}

import metadata._
import java.io.File
import models.sfdc.SfdcDescribeMetadata
import play.core.StaticApplication
import play.api.libs.json.JsNumber
import play.api.libs.json.JsArray
import play.api.libs.json.JsString


object ExhaustiveTest {
  // @todo Move this to utils, and use it also in DemoSpec?
  // http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1000000000.0 + "s")
    result
  }

  def main(args: Array[String]) {
//    implicit val application = new DefaultApplication(new File("."), this.getClass.getClassLoader, None, Mode.Dev)
    implicit val application = new StaticApplication(new File("."))

    lazy val sampleSfdcDescribeMetadataJson: String = {
//      import play.api.Play.current
//      Source.fromInputStream(Play.resourceAsStream("test/sfdc_metadata.sample.json").get).mkString("")
      scala.io.Source.fromFile("/Users/UPSHOT/dev/upshot/resources/test/sfdc_metadata.sample.json").mkString
    }
    lazy val sampleSfdcDescribeMetadata = {
      import models.mongoContext._
      grater[SfdcDescribeMetadata].fromJSON(sampleSfdcDescribeMetadataJson)
    }

    // Instead us the one pulled from Saleforce.
    val metadata = SfdcMetadata.metadata(sampleSfdcDescribeMetadata)
    val metadict = MetadictContext(metadata, Map[Seq[String], List[(CcgCat, SemanticState)]](), SfdcNQMetadata.generateNQMetadata(metadata))

    val physicalSqlGen = new SqlToSoqlGenerator(metadict.metadata)
    val columnSelector = new SfdcDefaultColumnsSelector(metadict.metadata)
    val parser = new NaturalQuery(metadict.dict, new SqlGen(metadict.nqmetadata, columnSelector), physicalSqlGen)

    import play.api.libs.json.Json

    for (exampleQuery <- exampleQueries) time {
      val result = parser.parse(exampleQuery)
      val r = result.chosenParseSqlStrings map {p => JsArray(Seq(JsString(p.parse.parseTokenString), JsString(p.sqlString), JsNumber(p.score)))}
      val o = Json.obj(
        "query" -> exampleQuery,
        "nBestParses" -> result.nBestParses,
        "results" -> r
//        "candidateQueries" -> result.candidateQueries,
//        "candidateSqls" -> result.candidateSqls
      )
      println(Json.toJson(o))
//      println(result.bestSqlStr.getOrElse("").trim)
    }
  }

  val exampleQueries = Array[String](
    "# of accounts in biotechnology",
    "Account Type = current",
    "Account Type equals current",
    "Account created this month",
    "Account new this month",
    "Account, Account Status equals Past",
    "Accounts in San Francisco",
    "Accounts, Account Status equals Past",
    "Companies where some deal is over $50K",
    "Companies with at least one opportunity over $50K",
    "Companies with largest headcount",
    "Companies with the most employees",
    "Company, city is San Francisco",
    "How many opportunities are there?",
    "Max, min, and average opportunity amount by industry",
    "Opportunity amount by industry",
    "Top 5 industries by total number of closed opportunities",
    "Top companies by headcount",
    "\"Account Type\" is current",
    "account 2013",
    "accounts by industry",
    "accounts greater than 60000",
    "accounts having amount greater than 60000",
    "accounts in one of the following industries: biotech, energy, banking",
    "accounts in the biotechnology industry",
    "accounts in the industry biotechnology",
    "accounts owned by Tom K",
    "accounts owned by any of the following: Tom K or Joseph T",
    "accounts with and without opportunities",
    "accounts with billing state CA",
    "accounts with billing state California",
    "accounts with billing state is CA",
    "accounts with employees > 3",
    "accounts with employees greater than 3",
    "accounts with industry biotechnology",
    "accounts with name Tom",
    "accounts with number of employees greater than 3",
    "accounts with opportunities with close date last year without opportunities with close date this year",
    "accounts with opportunities",
    "accounts without opportunities where amount is greater than 60000",
    "accounts without opportunities",
    "accounts",
    "all accounts",
    "any opportunity that is won",
    "any opportunity that was won",
    "any opportunity that's won",
    "average amount from opportunities",
    "average opportunity amount",
    "biotechnology accounts",
    "biotechnology industry accounts",
    "biotechnology opportunities",
    "bottom 5 opportunities by amount",
    "companies with opportunity value over $50K",
    "daus",
    "deals this month",
    "deals",
    "deleted opportunities",
    "e-Discovery Account",
    "female hispanic users",
    "female users",
    "how many accounts are there",
    "how many female hispanic users",
    "how many opportunities are there",
    "industry is biotechnology",
    "last five failed opportunities at companies with fewer than 5 employees",
    "leads this month",
    "monthly opportunity amount by close date",
    "monthly opportunity amount by create date",
    "monthly opportunity amount by created date",
    "monthly opportunity amount by creation date",
    "name is Thom",
    "newest biotechnology opportunities",
    "newest opportunities",
    "not won opportunities",
    "number of accounts",
    "number of biotechnology accounts",
    "number of deals in la by category",
    "number of leads",
    "number of rejected male to female friend requests",
    "oldest biotechnology opportunities",
    "oldest opportunities by close date",
    "oldest opportunities",
    "open opportunities related to biotechnology accounts",
    "open opportunities",
    "opportunities by name",
    "opportunities closed",
    "opportunities last year",
    "opportunities not closed",
    "opportunities not won",
    "opportunities over $180K",
    "opportunities over 180,000",
    "opportunities over 180000",
    "opportunities over 180K",
    "opportunities over one hundred and eighty thousand",
    "opportunities over one hundred eighty thousand",
    "opportunities over one-hundred eighty thousand",
    "opportunities related to biotechnology accounts",
    "opportunities that are not won",
    "opportunities that are won",
    "opportunities that aren't won",
    "opportunities that were not won",
    "opportunities that were won",
    "opportunities that weren't won",
    "opportunities where amount equals 60000",
    "opportunities where amount is 60000",
    "opportunities where amount is equal to 60000",
    "opportunities where amount is greater than 60000",
    "opportunities where name is Acme",
    "opportunities with amount at least 60000",
    "opportunities with amount at most 60000",
    "opportunities with amount greater than 60000",
    "opportunities with amount less than 60000",
    "opportunities with amount more than 60000",
    "opportunities with amount no more than 60000",
    "opportunities with amount smaller than 60000",
    "opportunities with close date last 2 days",
    "opportunities with close date last 2 fiscal quarters",
    "opportunities with close date last 2 fiscal years",
    "opportunities with close date last 2 months",
    "opportunities with close date last 2 quarters",
    "opportunities with close date last 2 weeks",
    "opportunities with close date last 2 years",
    "opportunities with close date last fiscal quarter",
    "opportunities with close date last fiscal year",
    "opportunities with close date last month",
    "opportunities with close date last quarter",
    "opportunities with close date last week",
    "opportunities with close date last year",
    "opportunities with close date next 2 days",
    "opportunities with close date next 2 fiscal quarters",
    "opportunities with close date next 2 fiscal years",
    "opportunities with close date next 2 months",
    "opportunities with close date next 2 quarters",
    "opportunities with close date next 2 weeks",
    "opportunities with close date next 2 years",
    "opportunities with close date next fiscal quarter",
    "opportunities with close date next fiscal year",
    "opportunities with close date next month",
    "opportunities with close date next quarter",
    "opportunities with close date next week",
    "opportunities with close date next year",
    "opportunities with close date this fiscal quarter",
    "opportunities with close date this fiscal year",
    "opportunities with close date this month",
    "opportunities with close date this quarter",
    "opportunities with close date this week",
    "opportunities with close date this year",
    "opportunities with close date today",
    "opportunities with close date tomorrow",
    "opportunities with close date within 2 days",
    "opportunities with close date yesterday",
    "opportunities with created date last 2 days",
    "opportunities with created date last 2 fiscal quarters",
    "opportunities with created date last 2 fiscal years",
    "opportunities with created date last 2 months",
    "opportunities with created date last 2 quarters",
    "opportunities with created date last 2 weeks",
    "opportunities with created date last 2 years",
    "opportunities with created date last fiscal quarter",
    "opportunities with created date last fiscal year",
    "opportunities with created date last month",
    "opportunities with created date last quarter",
    "opportunities with created date last week",
    "opportunities with created date last year",
    "opportunities with created date next 2 days",
    "opportunities with created date next 2 fiscal quarters",
    "opportunities with created date next 2 fiscal years",
    "opportunities with created date next 2 months",
    "opportunities with created date next 2 quarters",
    "opportunities with created date next 2 weeks",
    "opportunities with created date next 2 years",
    "opportunities with created date next fiscal quarter",
    "opportunities with created date next fiscal year",
    "opportunities with created date next month",
    "opportunities with created date next quarter",
    "opportunities with created date next week",
    "opportunities with created date next year",
    "opportunities with created date this fiscal quarter",
    "opportunities with created date this fiscal year",
    "opportunities with created date this month",
    "opportunities with created date this quarter",
    "opportunities with created date this week",
    "opportunities with created date this year",
    "opportunities with created date today",
    "opportunities with created date tomorrow",
    "opportunities with created date yesterday",
    "opportunities won",
    "opportunity amount by quarter over the last year",
    "quarterly opportunity amount by close date",
    "receipt headers by accounts",
    "show me the accounts",
    "sum of opportunity amount",
    "top 5 newest opportunities",
    "top 5 opportunities by amount",
    "top 5 opportunities",
    "top deals",
    "top five opportunities by amount",
    "top opportunities by industry",
    "top opportunities in technology at companies with fewer than five employees",
    "total amount from accounts",
    "total amount from all accounts",
    "total amount from open opportunities related to biotechnology accounts per stage",
    "total amount from opportunities with accounts per industry",
    "total amount from opportunities with accounts per stage",
    "total and average opportunity amount",
    "total bookings from wellness deals in la by subcategory",
    "total bookings from wellness deals in la per subcategory",
    "total bookings from wellness deals in la",
    "total closed opportunity amount per month in 2012",
    "total closed opportunity amount per month in January, 2012",
    "total contacts",
    "total daus by gender",
    "total daus by month",
    "total number of accounts",
    "total number of female hispanic users by country",
    "total number of female hispanic users in france",
    "total number of female hispanic users",
    "total number of opportunities",
    "total open opportunities",
    "total opportunities by close date",
    "total opportunity amount in the last 12 months",
    "total opportunity amount in the last twelve months",
    "total opportunity amount in the last year",
    "total opportunity amount last year",
    "total opportunity amount",
    "users",
    "value of deals",
    "weekly opportunity amount by close date",
    "wellness deals in la",
    "wellness deals",
    "won opportunities",
    "yearly opportunity amount by close date"
  )
}
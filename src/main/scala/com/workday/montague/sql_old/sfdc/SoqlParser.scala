/**
 * This should be fuzz-tested against SOQL that is and is not valid.
 */
package sfdc

import scala.util.parsing.combinator.RegexParsers
import sqlkit._

object SoqlParser extends RegexParsers {

  /**
   * Parse SOQL then re-render it.
   * @param soql
   */
  def roundtripSoql(soql: String): Option[String] = {
    val p = parseAll(QUERY_SELECTFROM_BLOB, soql)
    if (p.successful)
      Some(soqlGen.generateSql(p.get.asInstanceOf[Select]))
    else
      None
  }
  private[this] val soqlGen = new SoqlGenerator()

  /// Convention: t is a token. v is a value that is part of a token, and should be val, not def, because of whitespace.
  def QUERY_SELECTFROM: Parser[Select] = (tSELECT ~> SELECTFIELDLIST) ~
    (tFROM ~> FROMVAR) ~ (tCOMMA ~> JOINVAR).* ~
    (tWHERE ~> CONDITIONEXPR).? ~
    ((tGROUP ~> tBY ~> (tROLLUP | tCUBE) ~> tLPAREN ~> GROUPNAMELIST <~ tRPAREN) | (tGROUP ~> tBY ~> GROUPNAMELIST)).? ~
    (tHAVING ~> CONDITIONEXPR).? ~
    (tORDER ~> tBY ~> ORDERBYEXPR).? ~
    (tLIMIT ~> tPOSINTEGER).? ~
    (tFOR ~> tVIEW).? ^^ { case projs~from~joins~where~group~having~orderby~limit~for_ => sqlkit.Select(from=from, projs=projs, joins=joins)} // @todo: Parse other vals

  // Degenerate parser, only for SELECT and FROM clauses, other clauses (GROUP BY, ORDER BY, HAVING, etc.)
  // are preserved but only as globbed strings
  def QUERY_SELECTFROM_BLOB: Parser[Select] = (tSELECT ~> SELECTFIELDLIST) ~
    (tFROM ~> FROMVAR) ~ (tCOMMA ~> JOINVAR).* ~ ("""(.|\s)*""".r) ^^
    { case projs~from~joins~blobSql =>
      sqlkit.Select(from=from, projs=projs, joins=joins, blobSql=if (blobSql == "") None else Some(blobSql))}
  // @todo: use joins and rest and isAggregateQuery

  def SELECTFIELDLIST = (SELECTFIELDALIAS ~ (tCOMMA ~> SELECTFIELDALIAS).*) ^^ { case x~xseq => Seq(x) ++ xseq }
  // @bug Might not  all be Projections
  def SELECTFIELDALIAS = SELECTFIELD ~ ALIAS.? ^^ {
    case (s: Select) ~ a => Aliased(SelectProj(s), a)
    case s~a => Aliased[Proj](Proj(s.toString), a)
  }
  def SELECTFIELD = AGGREGATEFIELD | GROUPINGFIELD | tDATEFUNCTION | SUBSELECT | tNAME

  // We can't use QUERY_SELECTFROM_BLOB here because the blob part will capture things beyond the sub-select
  def SUBSELECT = (tLPAREN ~> QUERY_SELECTFROM <~ tRPAREN)

  def GROUPNAMELIST = (tDATEFUNCTION | tNAME) ~ (tCOMMA ~ (tDATEFUNCTION | tNAME)).*

  // @bug I believe these trip on 'select id from Account AS grouper'
  // @bug Also "select id from Account assignment" (changes it into "Account signment")
  // That's because the alias "grouper" begins with a restricted token, even though it's not the entire word.
  def FROMVAR = tNAME ~ (tAS.? ~> ALIAS).? ~ (tUSING ~> tNAME).? ^^ {
    case name~alias~using => Aliased(BaseRelation(name),alias)        // @todo: Use the using variable
  }
  def JOINVAR = tNAME ~ (tAS.? ~> ALIAS).? ~ (tUSING ~> tNAME).? ^^ {
    case name~alias~using => SoqlJoin(Aliased(name,alias))        // @todo: Use the using variable
  }

  // Rewrote this from the SOQL spec
  // @bug This won't necessarily parse AND and OR into the correct order-of-operations
  def CONDITIONEXPR: Parser[Any] = (BOOLEXPR ~ (tAND | tOR) ~ CONDITIONEXPR) | BOOLEXPR
  def BOOLEXPR = tNOT ~ CONDITIONEXPR | tLPAREN ~ tNOT.? ~ CONDITIONEXPR ~ tRPAREN | SIMPLEEXPR
  def SIMPLEEXPR = FIELDEXPR | SETEXPR

  // We include AGGREGATEFIELD and GROUPINGFIELD because the HAVING clause (but not WHERE) can use them.
  // We include tDATEFUNCTION because the WHERE (but not the HAVING) clause can use them.
  def FIELDEXPR = (AGGREGATEFIELD | GROUPINGFIELD | tDATEFUNCTION | tNAME) ~ tOPERATOR ~ VALUE
  def SETEXPR = tNAME ~ (tINCLUDES | tEXCLUDES | tIN | tNOT ~ tIN) ~ tLPAREN ~ VALUE ~ (tCOMMA ~ VALUE).* ~ tRPAREN
  def tOPERATOR = "=" | """\!=""" | "<=" | "<" | ">=" | ">" | "(?i)like".r

  // I added QUERY here, to capture subqueries
  def VALUE = QUERY_SELECTFROM | tNULL | tTRUE | tFALSE | tDATEFORMULA | tDATETIME | tDATE | tNUMBER | tSTRING_LITERAL

  def ORDERBYEXPR = ORDERBYSUBEXPR ~ (tCOMMA ~ ORDERBYSUBEXPR).*
  def ORDERBYSUBEXPR = (GROUPINGFIELD | tNAME) ~ (tASC | tDESC).? ~ (tNULLS ~ (tFIRST | tLAST)).?

//  def ALIAS = not(PROHIBITED_ALIASES ~ """\b""".r) ~ tNAME      // This doesn't work, but there's a bug that we only want to detect full-token prohibited aliases
  def ALIAS = not(PROHIBITED_ALIASES) ~> tNAME
  def PROHIBITED_ALIASES = tAND | tASC | tDESC | tEXCLUDES | tFIRST | tFROM | tGROUP | tHAVING | tIN | tINCLUDES | tLAST | tLIKE | tLIMIT | tNOT | tNULL | tNULLS | tOR | tSELECT | tWHERE | tWITH

  def GROUPINGFIELD = tGROUPING ~> tLPAREN ~> tNAME <~ tRPAREN ^^ { case name => s"GROUPING($name)" }    // Just generate a blob for now

  def tSELECT = "(?i)select".r
  def tFROM = "(?i)from".r
  def tAS = "(?i)as".r 
  def tUSING = "(?i)using".r 
  def tWHERE = "(?i)where".r
  def tGROUP = "(?i)group".r
  def tORDER = "(?i)order".r
  def tBY = "(?i)by".r
  def tLIMIT = "(?i)limit".r

  def AGGREGATEFIELD = AGGREGATECOUNTSTAR | AGGREGATEOTHER
  def AGGREGATECOUNTSTAR = tCOUNT ~ tLPAREN ~ tRPAREN ^^ { case _ => "COUNT()" }
  def AGGREGATEOTHER = AGGREGATE ~ (tLPAREN ~> tNAME <~ tRPAREN) ^^ { case agg~name => s"$agg($name)" }
  def AGGREGATE = (tCOUNT | tAVG | tCOUNT_DISTINCT | tMIN | tMAX | tSUM)
  def tCOUNT = """(?i)count""".r
  def tAVG = """(?i)avg""".r
  def tCOUNT_DISTINCT  = """(?i)count_distinct""".r
  def tMIN = """(?i)min""".r
  def tMAX = """(?i)max""".r
  def tSUM = """(?i)sum""".r
  /*
  def AGGREGATEFIELD = tCOUNT | tAVG | tCOUNT_DISTINCT | tMIN | tMAX | tSUM
  def tCOUNT = """(?i)count\(""".r ~ tNAME.? ~ ")"
  def tAVG = """(?i)avg\(""".r ~ tNAME ~ ")"
  def tCOUNT_DISTINCT  = """(?i)count_distinct\(""".r ~ tNAME ~ ")"
  def tMIN = """(?i)min\(""".r ~ tNAME ~ ")"
  def tMAX = """(?i)max\(""".r ~ tNAME ~ ")"
  def tSUM = """(?i)sum\(""".r ~ tNAME ~ ")"
  */

  def tHAVING = "(?i)having".r
  def tLIKE = "(?i)like".r
  def tWITH = "(?i)with".r
  def tROLLUP = "(?i)rollup".r
  def tCUBE = "(?i)cube".r
  def tGROUPING = "(?i)grouping".r
  def tFOR = "(?i)for".r
  def tVIEW = "(?i)view".r

  def tAND = "(?i)and".r 
  def tOR = "(?i)or".r 
  def tNOT = "(?i)not".r 

  def tINCLUDES = "(?i)includes".r 
  def tEXCLUDES = "(?i)excludes".r 
  def tIN = "(?i)in".r 

  def tASC = "(?i)asc".r 
  def tDESC = "(?i)desc".r 
  def tNULLS = "(?i)nulls".r 
  def tFIRST = "(?i)first".r 
  def tLAST = "(?i)last".r 

  def tDATEFORMULA = ("(?i)yesterday".r | "(?i)today".r | "(?i)tomorrow".r | "(?i)last_week".r | "(?i)this_week".r | "(?i)next_week".r | "(?i)last_month".r | "(?i)this_month".r | "(?i)next_month".r | "(?i)last_90_days".r | "(?i)next_90_days".r | "(?i)last_n_days:".r ~ vPOSINTEGER | "(?i)next_n_days:".r ~ vPOSINTEGER | "(?i)this_quarter".r | "(?i)last_quarter".r | "(?i)next_quarter".r | "(?i)next_n_quarters:".r ~ vPOSINTEGER | "(?i)last_n_quarters:".r ~ vPOSINTEGER | "(?i)this_year".r | "(?i)last_year".r | "(?i)next_year".r | "(?i)next_n_years:".r ~ vPOSINTEGER | "(?i)last_n_years:".r ~ vPOSINTEGER | "(?i)this_fiscal_quarter".r | "(?i)last_fiscal_quarter".r | "(?i)next_fiscal_quarter".r | "(?i)next_n_fiscal_quarters:".r ~ vPOSINTEGER | "(?i)last_n_fiscal_quarters:".r ~ vPOSINTEGER | "(?i)this_fiscal_year".r | "(?i)last_fiscal_year".r | "(?i)next_fiscal_year".r | "(?i)next_n_fiscal_years:".r ~ vPOSINTEGER | "(?i)last_n_fiscal_years:".r ~ vPOSINTEGER)

  def tDATEFUNCTION = vDATEFUNCTION ~ ("(" ~> tNAME.? <~ ")") ^^ { case datefn~name => s"$datefn($name)" }
  val vDATEFUNCTION = ("(?i)calendar_month".r | "(?i)calendar_quarter".r | "(?i)calendar_year".r | "(?i)day_in_month".r | "(?i)day_in_week".r | "(?i)day_in_year".r | "(?i)day_only".r | "(?i)fiscal_month".r | "(?i)fiscal_quarter".r | "(?i)fiscal_year".r | "(?i)hour_in_day".r | "(?i)week_in_month".r | "(?i)week_in_year".r)

  // The SOQL BNF doesn't allow numerals, but they seem to appear
  def tNAME = """[a-zA-Z][a-zA-Z0-9_\.]*""".r

  def tDATETIME = vYEAR ~ "-" ~ vMONTH ~ "-" ~ vDAY ~ "T" ~ vHOUR ~ ":" ~ vMINUTE ~ ":" ~ vSECOND ~ ("Z" | (("+" | "-") ~ vHOUR ~ ":" ~ vMINUTE))
  def tDATE = vYEAR ~ "-" ~ vMONTH ~ "-" ~ vDAY
  // We use vals, not defs, because these are part of tokens and we don't want to skip preceding whitespace
  // http://stackoverflow.com/questions/3347552/scala-parser-combinators-for-language-embedded-in-html-or-text-like-php
  val vYEAR = "[0-9][0-9][0-9][0-9]".r
  val vMONTH = "0[1-9]".r | "1[0-2]".r
  val vDAY = "0[1-9]".r | "[12][0-9]".r | "3[01]".r
  val vHOUR = "[01][0-9]".r | "2[0-3]".r
  val vMINUTE = "[0-5][0-9]".r
  val vSECOND = "[0-5][0-9]".r | "60"


  def tNULL = "(?i)null".r
  def tTRUE = "(?i)true".r
  def tFALSE = "(?i)false".r

  // Changed from SOQL BNF, because that only matches floats
  def tNUMBER = vINTEGER.? ~ "." ~ vPOSINTEGER | vINTEGER
  val vINTEGER = """[\+-]?""".r ~ vPOSINTEGER
  def tPOSINTEGER = vPOSINTEGER
  val vPOSINTEGER = "[0-9]+".r
  val vDIGIT = "[0-9]".r

  // These tokens don't need trailing whitespace
  def tCOMMA = ","
  def tLPAREN = "("
  def tRPAREN = ")"

  def tSTRING_LITERAL = "'" ~ (ESC_CHAR | ALMOST_ALL_CHARS).* ~ "'"
  val ALMOST_ALL_CHARS = """[^'\\]""".r
  val ESC_CHAR = """\\[nNrRtTbBfF"'\\]""".r
          // Also:
          //  LIKE expression only: \_ Matches a single underscore character ( _ )
          //  LIKE expression only:\% Matches a single percent sign character ( % )
          // pg. 3 of SOQL ref

  /*
  def apply(input: String): Double = parse(input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
  */

  def main(args: Array[String]) {
    /*
    val esc = "\\\\"
    val notEsc = "asdf"
    assert(parseAll(ESC_CHAR, esc).successful)
    assert(!parseAll(ESC_CHAR, notEsc).successful)

    assert(parseAll(ALMOST_ALL_CHARS, "a").successful)
    assert(!parseAll(ALMOST_ALL_CHARS, "aa").successful)
    assert(!parseAll(ALMOST_ALL_CHARS, "'").successful)
    assert(!parseAll(ALMOST_ALL_CHARS, "\\").successful)
    */

    assert(parseAll(tSTRING_LITERAL, "'string literal \\r'").successful)
    assert(!parseAll(tSTRING_LITERAL, "'string literal \\r").successful)
    assert(!parseAll(tSTRING_LITERAL, "'\\x'").successful)

    /*
    assert(parseAll(DIGIT, "0").successful)
    assert(!parseAll(DIGIT, "a").successful)

    assert(parseAll(INTEGER, "0").successful)
    assert(!parseAll(INTEGER, "-0.3").successful)
    assert(parseAll(INTEGER, "+2").successful)
    */

    assert(parseAll(tNUMBER, " -0.3").successful)

    assert(parseAll(tNULL, "null").successful)
    assert(!parseAll(tNULL, "true").successful)

    /*
    assert(parseAll(SECOND, "01").successful)
    assert(parseAll(SECOND, "59").successful)
    assert(parseAll(SECOND, "60").successful)
    assert(!parseAll(SECOND, "61").successful)

    assert(parseAll(MINUTE, "01").successful)
    assert(!parseAll(MINUTE, "60").successful)

    assert(parseAll(HOUR, "01").successful)
    assert(!parseAll(HOUR, "24").successful)

    assert(parseAll(DAY, "01").successful)
    assert(!parseAll(DAY, "32").successful)

    assert(parseAll(MONTH, "01").successful)
    assert(!parseAll(MONTH, "13").successful)
    */

    assert(parseAll(tDATETIME, "2005-10-08T01:02:03Z").successful)
    assert(parseAll(tDATETIME, "2005-10-08T01:02:03+04:30").successful)
    assert(parseAll(tDATETIME, "2005-10-08T01:02:03-12:00").successful)

    assert(parseAll(tDATEFORMULA, "YESTERDAY").successful)
    assert(parseAll(tDATEFORMULA, "NEXT_N_DAYS:5").successful)
    assert(!parseAll(tDATEFORMULA, "NEXT_N_DAYS:").successful)

    val soqlExamples2 = Array(
    "SELECT COUNT()\nFROM Contact, Contact.Account\nWHERE Account.Name = 'MyriadPubs'",
      "SELECT count()\nFROM Contact c, c.Account a\nWHERE a.name = 'MyriadPubs'",
    "SELECT count()\nFROM Contact c, c.Account a, Contact.Account, c.Account b\nWHERE a.name = 'MyriadPubs'"
    )
    val soqlExamples = Array(
      "SELECT Name, Position__r.Department__c FROM Job_Application__c\n",
      "SELECT Id\nFROM Account a",
      "SELECT count()\nFROM Account a",
      "SELECT count()\nFROM Account a\nWHERE a.name = 'MyriadPubs'",
      "SELECT count()\nFROM Contact c, c.Account a\nWHERE a.name = 'MyriadPubs'",
      "SELECT Id, Name\nFROM Account\nWHERE Name = 'Sandy'",
      "SELECT Id\nFROM Account\nWHERE Id NOT IN\n(\nSELECT AccountId\nFROM Opportunity\nWHERE IsClosed = false\n)",
      "SELECT Id\nFROM Task\nWHERE WhoId IN\n(\nSELECT Id\nFROM Contact\nWHERE MailingCity = 'Twin Falls'\n)",
      "SELECT Id, Name\nFROM Account\nWHERE Id IN\n( SELECT AccountId\nFROM Opportunity\nWHERE StageName = 'Closed Lost'\n)",
      "SELECT Id\nFROM Opportunity\nWHERE AccountId NOT IN\n(\nSELECT AccountId\nFROM Contact\nWHERE LeadSource = 'Web'\n)",
      "SELECT Id, Name\nFROM Account\nWHERE Id IN\n(\nSELECT AccountId\nFROM Contact\nWHERE LastName LIKE 'apple%'\n)\nAND Id IN\n(\nSELECT AccountId\nFROM Opportunity\nWHERE isClosed = false\n)",
      "SELECT Id, (SELECT Id from OpportunityLineItems)\nFROM Opportunity\nWHERE Id IN\n(\nSELECT OpportunityId\nFROM OpportunityLineItem\nWHERE totalPrice > 10000\n)",
      "SELECT Id\nFROM Idea\nWHERE (Id IN (SELECT ParentId FROM Vote WHERE CreatedDate > LAST_WEEK AND\nParent.Type='Idea'))",
      "SELECT Id\nFROM Idea\nWHERE (Idea.Title LIKE 'Vacation%')\nAND (Idea.LastCommentDate > YESTERDAY)\nAND (Id IN (SELECT ParentId FROM Vote\nWHERE CreatedById = '005x0000000sMgYAAU'\nAND Parent.Type='Idea'))",
      "SELECT Id\nFROM Account\nWHERE CreatedDate > 2005-10-08T01:02:03Z",
      "SELECT Name\nFROM Account\nORDER BY Name DESC NULLS LAST",
      "SELECT Name\nFROM Account\nWHERE Industry = 'Media' LIMIT 125",
    // We don't handle offset
//      "SELECT Name\nFROM Merchandise__c\nWHERE Price__c > 5.0\nORDER BY Name\nLIMIT 100\nOFFSET 10",
//      "SELECT Name, Id\n(\nSELECT Name FROM Opportunities LIMIT 10 OFFSET 2\n)\nFROM Account\nORDER BY Name\nLIMIT 1",
      "SELECT LeadSource, COUNT(Name)\nFROM Lead\nGROUP BY LeadSource",
      "SELECT Name, MAX(Amount), MIN(Amount)\nFROM Opportunity\nGROUP BY Name",
      "SELECT Name, MAX(Amount), MIN(Amount) min, SUM(Amount)\nFROM Opportunity\nGROUP BY Name",
      "SELECT LeadSource, COUNT(Name) cnt\nFROM Lead\nGROUP BY ROLLUP(LeadSource)",
      "SELECT LeadSource, Rating,\nGROUPING(LeadSource) grpLS, GROUPING(Rating) grpRating,\nCOUNT(Name) cnt\nFROM Lead\nGROUP BY ROLLUP(LeadSource, Rating)",
      "SELECT Type, BillingCountry,\nGROUPING(Type) grpType, GROUPING(BillingCountry) grpCty,\nCOUNT(id) accts\nFROM Account\nGROUP BY CUBE(Type, BillingCountry)\nORDER BY GROUPING(Type), GROUPING(BillingCountry)",
      "SELECT LeadSource, COUNT(Name)\nFROM Lead\nGROUP BY LeadSource",
      "SELECT LeadSource, COUNT(Name)\nFROM Lead\nGROUP BY LeadSource\nHAVING COUNT(Name) > 100",
      "SELECT Name, Count(Id)\nFROM Account\nGROUP BY Name\nHAVING Count(Id) > 1",
      "SELECT LeadSource, COUNT(Name)\nFROM Lead\nGROUP BY LeadSource\nHAVING COUNT(Name) > 100 and LeadSource > 'Phone'",
    // We don't support TYPEOF
//      "SELECT\nTYPEOF What\nWHEN Account THEN Phone, NumberOfEmployees\nWHEN Opportunity THEN Amount, CloseDate\nELSE Name, Email\nEND\nFROM Event",
      "SELECT Name, ID FROM Contact LIMIT 1 FOR VIEW",
      "SELECT CampaignId, AVG(Amount)\nFROM Opportunity\nGROUP BY CampaignId",
      "SELECT COUNT()\nFROM Contact, Contact.Account\nWHERE Account.Name = 'MyriadPubs'",
      "SELECT COUNT()\nFROM Account\nWHERE Name LIKE 'a%'",
      "SELECT COUNT(Id)\nFROM Account\nWHERE Name LIKE 'a%'",
      "SELECT COUNT()\nFROM Account\nWHERE Name LIKE 'a%'",
      "SELECT COUNT(Id)\nFROM Account\nWHERE Name LIKE 'a%'",
      "SELECT COUNT(Id), COUNT(CampaignId)\nFROM Opportunity",
      "SELECT LeadSource, COUNT(Name)\nFROM Lead\nGROUP BY LeadSource",
      "SELECT CALENDAR_YEAR(CreatedDate), SUM(Amount)\nFROM Opportunity\nGROUP BY CALENDAR_YEAR(CreatedDate)",
      "SELECT CreatedDate, Amount\nFROM Opportunity\nWHERE CALENDAR_YEAR(CreatedDate) = 2009",
    // Conversion and currencies not supported
//      "SELECT HOUR_IN_DAY(convertTimezone(CreatedDate)), SUM(Amount)\nFROM Opportunity\nGROUP BY HOUR_IN_DAY(convertTimezone(CreatedDate))",
//      "SELECT Id, convertCurrency(AnnualRevenue)\nFROM Account",
//      "SELECT Id, Name\nFROM Opportunity\nWHERE Amount > USD5000",
      "SELECT Name, MAX(Amount)\nFROM Opportunity\nGROUP BY Name\nHAVING MAX(Amount) > 10000",
      "SELECT Id, Name, BillingCity FROM Account",


      // Queries from Example SELECT Clauses section of SOQL ref
      "SELECT Id FROM Contact WHERE Name LIKE 'A%' AND MailingCity = 'California'",
      "SELECT Name FROM Account ORDER BY Name DESC NULLS LAST",
      "SELECT Name FROM Account WHERE Industry = 'media' LIMIT 125",
      "SELECT Name FROM Account WHERE Industry = 'media' ORDER BY BillingPostalCode ASC NULLS LAST LIMIT 125",
      "SELECT COUNT() FROM Contact",
      "SELECT LeadSource, COUNT(Name) FROM Lead GROUP BY LeadSource",
      "SELECT Name, COUNT(Id) FROM Account GROUP BY Name HAVING COUNT(Id) > 1",
      // Offset not supported
      //"SELECT Name, Id FROM Merchandise__c ORDER BY Name OFFSET 100",
      //"SELECT Name, Id FROM Merchandise__c ORDER BY Name LIMIT 20 OFFSET 100",
      "SELECT Contact.FirstName, Contact.Account.Name FROM Contact",
      "SELECT Id, Name, Account.Name FROM Contact WHERE Account.Industry = 'media'",
      "SELECT Name, (SELECT LastName FROM Contacts) FROM Account",
      "SELECT Account.Name, (SELECT Contact.LastName FROM Account.Contacts) FROM Account",
      "SELECT Name, (SELECT LastName FROM Contacts WHERE CreatedBy.Alias = 'x') FROM Account WHERE Industry = 'media'",
      "SELECT Id, FirstName__c, Mother_of_Child__r.FirstName__c FROM Daughter__c WHERE Mother_of_Child__r.LastName__c LIKE 'C%'",
      "SELECT Name, (SELECT Name FROM Line_Items__r) FROM Merchandise__c WHERE Name LIKE 'Acme%'",
      "SELECT Id, Owner.Name FROM Task WHERE Owner.FirstName like 'B%'",
      "SELECT Id, Who.FirstName, Who.LastName FROM Task WHERE Owner.FirstName LIKE 'B%'",
      "SELECT Id, What.Name FROM Event",
      // TypeOF not supported
      //"SELECT TYPEOF What WHEN Account THEN Phone, NumberOfEmployees WHEN Opportunity THEN Amount, CloseDate ELSE Name, Email END FROM Event ",
      "SELECT Name, (SELECT CreatedBy.Name FROM Notes) FROM Account",
      "SELECT Amount, Id, Name, (SELECT Quantity, ListPrice, PricebookEntry.UnitPrice, PricebookEntry.Name FROM OpportunityLineItems) FROM Opportunity",
      "SELECT UserId, LoginTime from LoginHistory",
      // I haven't checked why this doesn't work
      //"SELECT UserId, COUNT(Id) from LoginHistory WHERE LoginTime > 2010-09-20T22:16:30.000Z AND LoginTime < 2010-09-21T22:16:30.000Z GROUP BY UserId ",
      "SELECT Contact.FirstName, Contact.Account.Name from Contact",
      "SELECT Account.Name, (SELECT Contact.FirstName, Contact.LastName FROM Account.Contacts)\nFROM Account",
      "SELECT Id, Name, Account.Name\nFROM Contact\nWHERE Account.Industry = 'media'",
      "SELECT Name,\n(\nSELECT LastName\nFROM Contacts\n)\nFROM Account",
      "SELECT Name,\n(\nSELECT CreatedBy.Name\nFROM Notes\n)\nFROM Account",
      "SELECT Amount, Id, Name,\n(\nSELECT Quantity, ListPrice,\nPricebookEntry.UnitPrice, PricebookEntry.Name\nFROM OpportunityLineItems\n)\nFROM Opportunity",
      "SELECT Amount, Id, Name, (SELECT Quantity, ListPrice,\nPriceBookEntry.UnitPrice, PricebookEntry.Name,\nPricebookEntry.product2.Family FROM OpportunityLineItems)\nFROM Opportunity",
      "SELECT Name,\n(\nSELECT LastName\nFROM Contacts\nWHERE CreatedBy.Alias = 'x')\nFROM Account WHERE Industry = 'media'",
      "SELECT Id, FirstName__c, Mother_of_Child__r.FirstName__c\nFROM Daughter__c\nWHERE Mother_of_Child__r.LastName__c LIKE 'C%'",
      "SELECT LastName__c,\n(\nSELECT LastName__c\nFROM Daughters__r\n)\nFROM Mother__c",
      "SELECT Id, FirstName, LastName, AccountId, Account.Name\nFROM Contact\nWHERE Account.Name LIKE 'Acme%'",
      "SELECT Id, Name,\n(\nSELECT Id, FirstName, LastName\nFROM Contacts\n)\nFROM Account\nWHERE Name like 'Acme%'",
      "SELECT Id, CaseNumber, Account.Id, Account.Name\nFROM Case\nORDER BY Account.Name",
      "SELECT ID, Name, Parent__r.id, Parent__r.name\nFROM Child__c\nORDER BY Parent__r.name",
      "SELECT Id FROM Contact WHERE LastName = 'foo' or Account.Name = 'bar'",
      "SELECT Id\nFROM Case\nWHERE Contact.LastName = null",
      "SELECT Id, Owner.Name\nFROM Task\nWHERE Owner.FirstName like 'B%'",
      "SELECT Id, Who.FirstName, Who.LastName\nFROM Task\nWHERE Owner.FirstName LIKE 'B%'",
      "SELECT Id, Who.Id, Who.Type\nFROM Task",
      "SELECT Id, Who.Id, Who.Type\nFROM Task\nWHERE Who.Type='Contact'",
      "SELECT Id, What.Name\nFROM Event",
//      "SELECT\nTYPEOF What\nWHEN Account THEN Phone, NumberOfEmployees\nWHEN Opportunity THEN Amount, CloseDate\nELSE Name, Email\nEND\nFROM Event",
//      "SELECT\nTYPEOF What\nWHEN Account THEN Phone\nELSE Name\nEND\nFROM Event\nWHERE CreatedById IN\n(\nSELECT CreatedById\nFROM Case\n)",
      "SELECT Id\nFROM Event\nWHERE What.Type IN ('Account', 'Opportunity')",
//      "SELECT Id,\nTYPEOF What\nWHEN Account THEN Phone\nWHEN Opportunity THEN Amount\nEND\nFROM Event\nWHERE What.Type IN ('Account', 'Opportunity')",
      "SELECT Account.Name, (SELECT Note.OwnerId FROM Account.Notes) FROM Account",
      "SELECT Account.Name, (SELECT Note.Body FROM Account.Notes) FROM Account",
      "SELECT OldValue, NewValue, Parent.Id, Parent.name, Parent.customfield__c\nFROM foo__history",
      "SELECT Name, customfield__c, (SELECT OldValue, NewValue FROM foo__history)\nFROM foo__c",
      "SELECT Id,ParentId\nFROM Offer__DataCategorySelection",
      "SELECT Id, Title,\n(\nSELECT Id\nFROM DataCategorySelections\n)\nFROM Offer__kav WHERE publishStatus='online'",
      "SELECT Name, Location__latitude__s, Location__longitude__s\nFROM Warehouse__c",
      "SELECT Name, Location__c\nFROM Warehouse__c",
    // We don't support DISTANCE queries
//      "SELECT Name, Location__c\nFROM Warehouse__c\nWHERE DISTANCE(Location__c, GEOLOCATION(37.775,-122.418), 'mi') < 20",
//      "SELECT Name, StreetAddress__c\nFROM Warehouse__c\nWHERE DISTANCE(Location__c, GEOLOCATION(37.775,-122.418), 'mi') < 20\nORDER BY DISTANCE(Location__c, GEOLOCATION(37.775,-122.418), 'mi')\nLIMIT 10",
      "SELECT Name, Location__c\nFROM Warehouse__c\nLIMIT 5",
      "SELECT Name, Location__latitude__s, Location__longitude__s\nFROM Warehouse__c\nLIMIT 5",
    

      "SELECT Name, (SELECT Name FROM Job_Applications__r) FROM Position__c\n",
      "SELECT Name FROM Position_c WHERE Id IN\n\n(SELECT Position__c FROM\tJob_Application__c)",
      "\nSELECT Name,Position__r.Name\n\nFROM Job_Application__c\n\nWHERE Position__r.Department__c = 'Sales'",
      "SELECT Name FROM Position_c WHERE Id NOT IN\n\n(SELECT Position__c FROM Job_Application__c)",
      "SELECT Name FROM Job_Application__c WHERE Position__c = null\n",
      "SELECT Position__r.Department__c deptname, COUNT(id) total\n\nFROM Job_Application__c\n\nGROUP BY Position__r.Department__c",
      "SELECT Title FROM Position__c WHERE CALENDAR_MONTH(Date_Closed__c) = 2\n",
      "SELECT Position__r.Department__c deptname, COUNT(id) total\n\nFROM Job_Application__c\n\nGROUP BY ROLLUP(Position__r.Department__c)",
      "SELECT LeadSource, COUNT(Name) cnt\nFROM Lead\nGROUP BY ROLLUP(LeadSource)",
      "SELECT Status, LeadSource, COUNT(Name) cnt\nFROM Lead\nGROUP BY ROLLUP(Status, LeadSource)",
      "SELECT Type, BillingCountry,\n    GROUPING(Type) grpType, GROUPING(BillingCountry) grpCty,\n    COUNT(id) accts\nFROM Account\nGROUP BY CUBE(Type, BillingCountry)\nORDER BY GROUPING(Type), GROUPING(BillingCountry)",
      "SELECT LeadSource, Rating,\n    GROUPING(LeadSource) grpLS, GROUPING(Rating) grpRating,\n    COUNT(Name) cnt\nFROM Lead\nGROUP BY ROLLUP(LeadSource, Rating)"

      // This breaks, because the alias begins with a keyword
      //"select id from Account AS grouper"
    )

    for ((soql, i) <- soqlExamples.zipWithIndex) {
      println("----------------------")
      println("Test #", i)
      println
      println(soql)
      println
/*
      val r = parseAll(QUERY, soql)
      println(r.successful)
      println(r)
//      println(flatten(r))
      println
      println(sqlGen.generateSql(r.get.asInstanceOf[Select]))
      if (!r.successful) throw new Exception("FAILURE")

      println("----------------------")
      */
      val r2 = parseAll(QUERY_SELECTFROM_BLOB, soql)
      println(r2.successful)
//      println(r2)
      println(soqlGen.generateSql(r2.get.asInstanceOf[Select]))


    }
  }
}

package natural

import metadata._
import sqlkit._
import parser.{SemanticParseNode, NonTerminal, SemanticParseResult}
import scala.collection.mutable
import play.api.templates.HtmlFormat
import util.Logging
import sfdc.TokenizeQuotedString


/**
 * "NaturalQuery" is the highest-level parsing service just below the MVC layer.  It combines the following
 * components into a single end-to-end pipeline:
 *    - tokenization                    string => tokens
 *    - semantic parsing, CCG           tokens => semantic parses
 *    - Logical SQL generation          semantic parse => sql AST (sqlkit.Select)
 *    - Physical SQL generation         sql AST => string
 *    - Best parse SQL selection        list of (parse, logical SQL) -> (parse, logical SQL)
 *
 * We take as input a dictionary as well as an implementation for each component of the pipeline.
 *
 * In addition to the (multiple, ambiguous) parses, we can also output unrecognized tokens from the parse.
 *
 * @param starDictGenerator a function that generates a star dict as a function of the semantic parse result of the main parse
 *                          of the query,
 *                          or None to use the default star dict, which is the entire dictionary
 */
class NaturalQuery(dict: NQDict, sqlgen: SqlGen, physicalSqlGen: SqlStringGenerator,
                   starDictGenerator: Option[SemanticParseResult[_] => NQDict] = None) extends Logging {

  private[this] val DEFAULT_TIME_LIMIT_SECS = 120.0

  /**
   * @param allowStarParse fallback to star parser if main parse doesn't result in oneBest
   * @param timeLimitSecs Overall time limit for parsing (including both main and star parse)
   */
  def parse(str: String, allowStarParse: Boolean = false, timeLimitSecs: Double = DEFAULT_TIME_LIMIT_SECS) = {
    // Tokenize
    val startTime = System.nanoTime()
    val origTokens = inputToTokens(str)
    val (nParsesGenerated, semanticParseResult, origParseSql) = parseFromTokens(origTokens, timeLimitSecs = timeLimitSecs)
    val unrecognizedTokensIdx = semanticParseResult.findUnrecognizedAs(nodeUnrecognized)

    val baseParseSteps = semanticParseResult.totalChartSize
    var totalParseSteps = baseParseSteps
    val origParseSqlStrings = generateSqlStrings(origParseSql)
    val origParseResult = resultsFromCandidates(origTokens, origParseSqlStrings, unrecognizedTokensIdx, false, nParsesGenerated)
    // See if the original string had a single parse
    val result =
      if (origParseResult.oneBest || origParseResult.ambiguous || !allowStarParse) {
        // If so, just keep the original string parses
        origParseResult
      } else {
        // Otherwise, parse the star queries
        var alternateSemanticParseResults = new mutable.ListBuffer[SemanticParseResult[CcgCat]]
        var alternateParseSql = new mutable.ListBuffer[ParseSql]
        var alternateNParsesGenerated: Int = 0
        val starDict = starDictGenerator.map(_(semanticParseResult))
        if (!starDict.isEmpty) {
          /**
           * Do star parsing by inserting a *TRACE* token.
           * However, time execution and cut off star parsing if it takes more than MAX_SECONDS_FOR_STAR_PARSER seconds.
           */
          object ExecutionTimeOverrun extends Exception { }
          val tStart = System.currentTimeMillis
          try {
            for (alternateTokens <- tokensToStarTokens(origTokens)) {
              val (newNParsesGenerated, newSemanticParseResult, newParsesWithSql) = parseFromTokens(alternateTokens, starDict, timeLimitSecs)
              alternateSemanticParseResults += newSemanticParseResult
              alternateParseSql ++= newParsesWithSql
              alternateNParsesGenerated += newNParsesGenerated
              totalParseSteps += newSemanticParseResult.totalChartSize
              val tEnd = System.currentTimeMillis
              if (timeLimitSecs < (tEnd - tStart)/1000.0) throw ExecutionTimeOverrun
            }
          } catch {
            case ExecutionTimeOverrun => logger.warn("Execution time overrun when running star-parser on " + origTokens.toList.toString)
          }

          val tmpAllParseSql = origParseSql ++ alternateParseSql.toList
          // @todo It would be slightly more efficient not to parseSqlStringsFromRenders on origParseSqlAndRenders.
          // Only call it on alternateParseSqlAndRenders and concatenate with origParseSqlStrings.
          // However, this will make the code only 5% faster in the worst case, so forget it for now.
          val tmpAllParseSqlStrings = generateSqlStrings(tmpAllParseSql)
          val (allParseSqlStrings, allParseSql) = (tmpAllParseSqlStrings, tmpAllParseSql)

          val starParseResult = resultsFromCandidates(origTokens, allParseSqlStrings,
            unrecognizedTokensIdx, true, alternateNParsesGenerated)
          starParseResult
        } else {
          origParseResult
        }
      }
    val durationSecs = (System.nanoTime() - startTime) / 1000.0 / 1000.0 / 1000.0
    logger.info("Parse time in secs = " + durationSecs + ", base parse steps = " + baseParseSteps + ", total parse steps = " + totalParseSteps + ", parse steps/sec = " + (totalParseSteps / durationSecs))
    result
  }

  def parseToChart(str: String, timeLimitSecs: Double = DEFAULT_TIME_LIMIT_SECS) = {
    // Tokenize
    val origTokens = inputToTokens(str)
    val (nParsesGenerated, semanticParseResult, origParseSqlAndRenders) = parseFromTokens(origTokens, timeLimitSecs = timeLimitSecs)
    semanticParseResult
  }

  /**
   * @param str The input string
   * @return tokens
   */
  private[this] def inputToTokens(str: String): Array[String] = {
    val s = str.toLowerCase
    val parseResult = TokenizeQuotedString.parseResult(s)
    if (parseResult.successful) {
      parseResult.get.toArray
    } else {
      // If we can't tokenize correctly (e.g. they use double-quotes in weird ways), fall back to simple whitespace tokenization.
      // @todo: We actually should just give them an error message.
      s.split("\\s+")
    }
  }

  private[this] def tokensToStarTokens(origTokens: Array[String]) = {
    for (traceIdx <- 0 to origTokens.length) yield {
      origTokens.slice(0, traceIdx) ++ Seq("*TRACE*") ++ origTokens.slice(traceIdx, origTokens.length)
    }
    /**
     * We used to also do substitutions, replacing one or two (contiguous) tokens with a *SUB1* or *SUB2* token.
     * The substitution parses don't seem to help much and double the time for star parsing:
     * - Doesn't produce results nearly as often as trace parsing
     * - When it does produce results, they're frequently wrong
     */
  }

  private[this] def parseFromTokens(tokens: Array[String], starDict: Option[NQDict] = None, timeLimitSecs: Double) = {
    // CKY
    val parser = new NaturalQueryParser(dict, starDict, timeLimitSecs)
    val semanticParseResult = parser.parse(tokens)
    val nParsesGenerated = semanticParseResult.chart(0, semanticParseResult.chart.n-1).length

    // LF => Logical SQL
    val parsesWithSql: List[(NQParseNode, Select)] = semanticParseResult.translateLogicalForms(sqlgen.generate(_: LFNode))
    // @todo Can we avoid doing this next step, and instead do it in semanticParseResult.translateLogicalForms?
    logger.debug(s"#parses = $nParsesGenerated, #(parse,sql) = ${parsesWithSql.length}")
    logger.trace(semanticParseResult.semanticCompleteParses.mkString("\n"))

    val parseSql = parsesWithSql.map(x => new ParseSql(x._1, x._2))
    (nParsesGenerated, semanticParseResult, parseSql)
  }

  /*
  def parseToLogicalForm(str: String): List[LFNode] = {
    parse(str).logicalForms
  }
  */

  def resultsFromCandidates(originalTokens: IndexedSeq[String], allParseSqlStrings: List[ParseSqlString],
                            unrecognizedTokensIdx: IndexedSeq[Int],
                            usedStarParser: Boolean, nParsesGenerated: Int) = {
    // Ignore the prominentDateColumn
    val filteredParseSqlStrings = allParseSqlStrings.filterNot(p => p.sqlString.contains("$prominent"))

    logger.debug("# original candidate parses: " + allParseSqlStrings.length)
    logger.debug("# filtered candidate parses: " + filteredParseSqlStrings.length)

    val parseSqlStrings = filteredParseSqlStrings

    if (parseSqlStrings.isEmpty) {
      logger.debug("# best parses: " + 0)
      new NQParseResult(originalTokens, Nil, unrecognizedTokensIdx,
        usedStarParser = usedStarParser, nSemanticParsesGenerated = nParsesGenerated,
        nSqlParsesGenerated = filteredParseSqlStrings.length)
    } else {
      // Find the parseSqlString with the highest score.

      val bestParseScore = (parseSqlStrings maxBy { parseSqlString => parseSqlString.score }).score
      val allBestParses = parseSqlStrings.filter(p => p.score == bestParseScore)

      // Now, group the best parses by the SQL they generate
      // This also sucks because it shuffles the order.
      val sqlToBestParses = allBestParses.groupBy{_.sqlString}
      // And now keep only parse per SQL
      // @todo Use more intelligence to pick with parse to keep, don't just pick head.
      // @todo Potential bug: If one query generates MULTIPLE SQLs, it will appear twice in the output still.
      val bestParses = sqlToBestParses.map{_._2.head}.toList

      /*
      // Let's also uniq by the query string.
      // This isn't ideal because sometimes a query has two parses, e.g. 'deals d'
      val queryToBestParses = bestParsesUniqSql.groupBy{_.parse.parseTokenString}
      val bestParses = queryToBestParses.map{_._2.head}.toList
      */

      logger.debug("# best parses: " + bestParses.length)
      logger.info(s"Best parse score = ${bestParseScore}")
      logger.info(s"Best parses = ")
      for (p <- bestParses) {
        logger.info(s"   ${p.parseTokenString}")
      }
      /*
      for (t <- bestParses(0).parse.terminals) {
        println(t.asInstanceOf[NQTerminal].sem.toString)
        println(t.asInstanceOf[NQTerminal].source)
      }
      */

        /// @todo: Passing in allParseSqlAndRenders is pretty lame, just so we can associate a RowRender with a ParseSqlString.
      val chosenParseWithSqls = bestParses map {
        choice =>
          //val chosenRender = allParseSql.find(_.parse == choice.parse).get._2
          new ParseSqlString(choice.parse, choice.sql, physicalSqlGen.generateSql(choice.sql))
      }

      for (parseSqlString <- chosenParseWithSqls) {
        logger.trace("Candidate parse: " + parseSqlString.toString.replace("\n", "\\n"))
        logger.trace(parseSqlString.parseTokens.toString)
        //      println("AA " + parseSqlString.toString)
      }

      new NQParseResult(originalTokens, chosenParseWithSqls, unrecognizedTokensIdx,
        usedStarParser=usedStarParser, nSemanticParsesGenerated=nParsesGenerated,
        nSqlParsesGenerated=filteredParseSqlStrings.length)
      /*
      // Get result
      new NQParseResult(semanticParseResult, alternateSemanticParseResults.toList, parseSqls, chosenParseWithSql)
      */

    }
  }

  /**
   * A token is recognized if:
   * - it came directly from the main dictionary (i.e. not a fallback i.e. not a Literal)
   * - it's part of a phrase (a span) that came directly from the main dictionary (i.e. not a fallback i.e. not a Literal)
   * - it's a literal that's captured as part of a Done parse ('name is Tom K', 'named Tom K')
   * - unfortunately that would currently capture 'amount is greater than Tom K' because "greater than Tom K" would be a literal and "amount is X" would be a Done parse
   *   - but we can fix that later and this just causes us to NOT show errors where we should rather than showing a false positive
   *
   *  This only makes sense against the main parse.
   *  @todo Think through if this makes sense with multiple parses.
   */
  val nodeUnrecognized = { node: SemanticParseNode[CcgCat] =>
    node match {
      case n: NQTerminal => !n.isFallback
      case NonTerminal(_, Done(_), _, _) => true
      case _ => false
    }
  }

  /// Find all ParseSqls that can be converted to strings
  /// @todo This is ugly and we'll have to clean it up later
  def generateSqlStrings(parseSqls: List[ParseSql]): List[ParseSqlString] = {
    val parseSqlStrings: List[ParseSqlString] = parseSqls.flatMap { parsesql =>
      try {
        val sqlString = physicalSqlGen.generateSql(parsesql.sql)
        Some(new ParseSqlString(parsesql.parse, parsesql.sql, sqlString))
      } catch {
        case e: Exception =>
          logger.warn("Error while generating sql for: " + parsesql.parse.toString, e)
          None
      }
    }
    parseSqlStrings
  }
}

class ParseSql (
  val parse: NQParseNode,
  val sql: sqlkit.Select
) {
  def parseTokens = parse.parseTokens
  /// parseTokenString is the literal string of ParseTokens, even if there are star tokens (*TRACE*, etc.) included.
  def parseTokenString = parse.parseTokenString
  /// queryString is the end-user version of the query: a string of ParseTokens with star tokens (*TRACE*, etc.) stripped.
  def queryString = parseTokens.map{ parseString =>
    if (Array[String]("*TRACE*", "*SUB1*", "*SUB2*") contains parseString.tokens.head) {
      parseString.tokens.tail.mkString(" ")
    } else {
      parseString.tokens.mkString(" ")
    }
  }.mkString(" ")
  /// queryHtml is the queryString, but with "<em>" blocks around each star token.
  /// @todo Make an isStarToken method?
  def queryHtml = parseTokens.map{ parseString =>
    if (Array[String]("*TRACE*", "*SUB1*", "*SUB2*") contains parseString.tokens.head) {
      (s"<em>${HtmlFormat.escape(parseString.tokens.tail.mkString(" "))}</em>")
    } else {
      (s"${HtmlFormat.escape(parseString.tokens.mkString(" "))}")
    }
  }.mkString(" ")

}

class ParseSqlString (
  parse: NQParseNode,
  sql: sqlkit.Select,
  val sqlString: String
) extends ParseSql(parse, sql) {

  val bestPossibleScore = 0
  /**
   * Given a parse and its SQL, assign a score to them.
   */
  def score = {
    // For each length of literals (# tokens), count how many times this occurs in the parse
    var literalLengthCnt = mutable.Map[Int,Int]().withDefaultValue(0)
    var literalCnt = 0
    for (tm <- parse.terminals) {
      val t = tm.asInstanceOf[NQTerminal]
      if (t.isLiteral) {
        literalLengthCnt(t.spans.length) += 1
        literalCnt += t.spans.length
      }
    }
    var traceCnt = 0
    var sub1Cnt = 0
    var sub2Cnt = 0
    for (t <- parseTokens) {
      if (t.tokenString.startsWith("*TRACE*")) traceCnt += 1
      if (t.tokenString.startsWith("*SUB1*")) sub1Cnt += 1
      if (t.tokenString.startsWith("*SUB2*")) sub2Cnt += 1
    }
    // TODO: Also penalize synonyms traces?
    // TODO: Also include the SQL string length?

    /*
    println(traceCnt)
    println(literalCnt)
    println(sub1Cnt)
    println(sub2Cnt)
    println()
    */

    // Not recognizing a word is twice as bad as adding a new word.
    val s = - traceCnt - 2*(literalCnt + sub1Cnt + 2*sub2Cnt)
  //  val s = 1.0/sqlString.length - literalCnt -
    assert (s <= bestPossibleScore)
    s
  }
  /// True iff this parse has the best possible score, i.e. no loss was incurred.
  def isBestScore = (score == bestPossibleScore)


    /// sql doesn't have a proper toString method, so ignore it.
  override def toString = s"{parse: $parse.toString, sqlString: $sqlString}"
//  override def toString = s"{parse: $parse.toString, sql: $sql.toString, sqlString: $sqlString}"
}

/**
 * The result of parsing a query.
 * @param originalTokens The original tokens in the query, *without* the addition of trace tokens or substitutions.
 * @param chosenParseSqlStrings A list of successful parses.
 *                              Length 0 = No successful parses
 *                              Length 1 = One, unambiguous parse
 *                              Length >1 = Several ambiguous parse results.
 */
class NQParseResult(val originalTokens: IndexedSeq[String], val chosenParseSqlStrings: List[ParseSqlString],
                    val unrecognizedTokensIdx: IndexedSeq[Int], val usedStarParser: Boolean,
                    val nSemanticParsesGenerated: Int, val nSqlParsesGenerated: Int) {
//  def successful = chosenParseSqlString.isDefined

  def unsuccessful: Boolean = nBestParses == 0
  def oneBest: Boolean = nBestParses == 1
  def ambiguous: Boolean = nBestParses > 1

  def nBestParses = chosenParseSqlStrings.length

  def bestParseSqlString: Option[ParseSqlString] = {
    if (oneBest || (!usedStarParser && ambiguous)) Some(chosenParseSqlStrings(0))
    else None
  }

  def bestParse: Option[NQParseNode] = bestParseSqlString.map(_.parse)
  def bestLogicalSql: Option[sqlkit.Select] = bestParseSqlString.map(_.sql)
  def bestSqlStr: Option[String] = bestParseSqlString.map(_.sqlString)

  // (query parseTokens, semantic parse (as string), SQL (as string), score)
  def bestQueryParseSqlScore: IndexedSeq[(IndexedSeq[IndexedSeq[String]], String, String, Float)] =
    chosenParseSqlStrings.map(p => (
      p.parse.parseTokens.map(pt => pt.tokens.toIndexedSeq).toIndexedSeq,
      p.parse.toString, p.sqlString, p.score.toFloat
      )).toIndexedSeq

  def mapOneBest[T](fn: ParseSqlString => T): Option[T] = {
    bestParseSqlString map { parsesqlstring =>
      fn(parsesqlstring)
    }
  }

  def candidateQueriesTokens = {
    chosenParseSqlStrings map (p => p.parse.parseTokens)
  }

  def uniqueCandidateQueriesTokens = {
    // Convert to Seq, to preserve the order
    candidateQueriesTokens.toSeq.distinct.toList
  }

  def candidateQueryHtmls = chosenParseSqlStrings map (p => p.queryHtml)

  /*
  /// Map from sql string to all candidate queries that generate this SQL
  def candidateSqlsToQueries = {
    var m = new mutable.HashMap[String, mutable.Seq[String]]()
    for (p <- chosenParseSqlStrings) {
      m(p._1.sqlString) += Seq[String](p._1.parse.parseTokenString)
    }
  }                                   */

  /// @todo Think through if this makes sense with multiple parses.
  val unrecognizedTokens = {
    unrecognizedTokensIdx.map { idx => originalTokens(idx)}
  }
}


// Basic grammatical role templates i.e. nouns, verbs, adjectives in the manner of CCG types (forward and backwards)

// CcgCategory should be classes that carry semantic labels that can be used later to create semantic trees
// for now the semantic label can be a String say (or we can use subclass types)
// but in the future we might parameterize CcgCategory to CcgCategory[LogicalTagType]
// or something

package cky

import scala.reflect.ClassTag

/**
 * A generic CKY parser that fills a chart with cells of type C.
 *
 * C is a semi-ring.  See;
 *
 * Goodman (1999): http://acl.ldc.upenn.edu/J/J99/J99-4004.pdf
 * http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.98.2791&rep=rep1&type=pdf
 * https://github.com/scalanlp/breeze/issues/21
 *
 * TODO: parameterize over parseToken type
 * @tparam C cell type
 */
trait CkyParser[C] {
  /**
   * Time limit before aborting parsing
   */
  protected def timeLimitSecs: Double

  protected def newEntry(parseToken: ParseToken, spans: Spans): C

  protected def concat(a: C, b: C): C

  protected def coalesce(left: C, right: C): C

  def parseToChart(tokens: IndexedSeq[String])(implicit ev: ClassTag[C]): Chart[C] = {
    val startTimeNano = System.nanoTime()
    val chart = new Chart[C](tokens)
    // logger.debug("parse(tokens " + tokens + ")")
    var abort = false
    for(spanLen <- 1 to tokens.length) {
      for(iStart <- 0 to tokens.length - spanLen) {
        val iEnd = iStart + spanLen - 1
        // We need to invoke newEntry even when abort = true because otherwise the chart
        // contains null and we get NPE's e.g. when callers ask for the final parses
        chart(iStart, iEnd) = newEntry(ParseToken(tokens = tokens.slice(iStart, iEnd + 1)), Spans(iStart, iEnd+1))
        if (!abort) {
          // For this span, consider all binary productions
          for (iPartition <- iStart + 1 to iEnd) {
            // Note when spanLen = 1, this never happens
            val left = chart(iStart, iPartition - 1)
            val right = chart(iPartition, iEnd)
            // cell contents += coalesce(left span, right span)
            chart(iStart, iEnd) = concat(chart(iStart, iEnd), coalesce(left, right))
          }
          postStep(chart, tokens, iStart, iEnd)
        }
        val durationSecs = (System.nanoTime() - startTimeNano) / (1000.0 * 1000.0 * 1000.0)
        if (durationSecs > timeLimitSecs && !abort) {
          // logger.error("Parsing " + tokens + " exceeded time limit of " + timeLimitSecs + " secs, aborting parse and returning partially parsed chart!")
          abort = true
        }
      }
    }
    // logger.debug("final chart:\n" + chart.toString)
    chart
  }

  def parse(tokens: IndexedSeq[String])(implicit ev: ClassTag[C]): C = {
    val matrix = parseToChart(tokens)
    matrix(0, tokens.length - 1)
  }

  protected def postStep(matrix: Chart[C], tokens: IndexedSeq[String], iStart: Int, iEnd: Int) { }

}

/**
 * A CKY parser, where each cell is list of constituents that can be derived.
 *
 * @tparam T constituent type
 */
trait CkyParserWithList[T] extends CkyParser[List[T]] {
  protected def newEntry(parseToken: ParseToken, spans: Spans): List[T] = dictLookup(parseToken, spans: Spans)

  protected def dictLookup(parseToken: ParseToken, spans: Spans): List[T]

  protected def concat(a: List[T], b: List[T]): List[T] = a ++ b

  protected def coalesce(left: List[T], right: List[T]): List[T] = {
    // println(left, right)
    for(leftEntry <- left;
        rightEntry <- right;
        newEntry <- derive(leftEntry, rightEntry))
    yield newEntry
  }

  override protected def postStep(matrix: Chart[List[T]], tokens: IndexedSeq[String], iStart: Int, iEnd: Int) {
    if (matrix(iStart, iEnd).isEmpty) {
      matrix(iStart, iEnd) = fallbackEntry(ParseToken(tokens.slice(iStart, iEnd + 1)), Spans(iStart, iEnd + 1))
    }
  }

  protected def fallbackEntry(parseToken: ParseToken, spans: Spans): List[T] = Nil

  protected def derive(left: T, right: T): List[T]
}
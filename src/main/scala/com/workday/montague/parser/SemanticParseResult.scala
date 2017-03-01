package com.workday.montague.parser

import com.workday.montague.ccg.SyntacticLabel
import com.workday.montague.cky.Chart
import com.workday.montague.semantics._

import scala.reflect.ClassTag

/**
 * Package together the result of a semantic parse:
 *  - the final semantic parse trees
 *  - intermediate results and helper methods.
 */
class SemanticParseResult[S <: SyntacticLabel[S]](val tokens: IndexedSeq[String], val chart: Chart[List[SemanticParseNode[S]]]) {
  def parses = chart(0, chart.n - 1)

  def bestParse = {
    parses.sortBy(node => -node.syntactic.score)
      .headOption
  }

  def semantics: SemanticState = {
    val semanticResults = semanticCompleteParses.map(_.semantic).toSet
    println
    semanticResults.size match {
      case 0 => Nonsense()
      case 1 => semanticResults.head
      case _ => Ambiguous(semanticResults)
    }
  }

  def totalChartSize = chart.allCells.map(_._2.size).sum

  def allChartParseNodes = chart.allCells.map(_._2).flatten

  def allChartCompleteSemantics[LF : ClassTag] = allChartParseNodes.map(_.semantic).collect { case Form(lf: LF) => lf }
  /**
   * @return Indexes of unrecognized tokens.
   */
  def findUnrecognizedAs(recognized: SemanticParseNode[S] => Boolean) = {
    findEventuallyRecognizedAs(chart, recognized)
      .zipWithIndex                         // form list of (recognized, index) pairs
      .filter(_._1 == false)                // take the unrecognized ones
      .map(unrecog => unrecog._2)   // splice the corresponding tokens by index
  }

  def semanticCompleteParses = {
    parses.filter(_.isSemanticComplete)
  }

  /**
   * Take all semantic complete parses, zipMap them using a function trans: LF => Option[T] over the logical form, then
   * filter only the ones where trans returned Some(gen)
   *
   * @param trans a function that translates the logical form LF to Option[T]
   */
  def translateLogicalForms[LF : ClassTag, T](trans: LF => Option[T]): List[(SemanticParseNode[S], T)] = {
    semanticCompleteParses
      .map(parse => (parse, parse.logicalFormOption.flatMap(trans)))
      .collect({ case (parse, Some(gen)) => (parse, gen) })
  }

  /**
   * Find all indices i, in which we have parses for Spans (0, i+1) and (i+1, n).
   * i.e. all position in which, if we insert a trace, we might be able to parse
   * the entire query.
   */
  def twoCoverings = {
    val n = chart.n

    (0 until n-1).flatMap(i =>
      if (chart(0,i).nonEmpty && chart(i+1, n-1).nonEmpty)
        Some(i+1)
      else
        None
    )
  }

  // Debug output
  def debugPrint() {
    val n = chart.n
    print("     ")
    for(iEnd <- 0 until n) {
      print("%5d".format(iEnd+1))
    }
    println()
    for(iStart <- 0 until n) {
      print("%5d".format(iStart))
      for(iEnd <- 0 until n)
        if (iEnd < iStart)
          print("     ")
        else
          print("%5d".format(chart(iStart,iEnd).length))
      println()
    }
    println()
    // Now print the two-coverings
    for(i <- 0 until n-1) {
      if (chart(0,i).nonEmpty && chart(i+1, n-1).nonEmpty)
        println(s"2-covering parse with break after $i")
    }

    // Now print the number of empty spans covering each token
    val emptyCoveringsPerToken = scala.collection.mutable.ArrayBuffer.fill(n)(0)
    val totalCoveringsPerToken = scala.collection.mutable.ArrayBuffer.fill(n)(0)
    println(emptyCoveringsPerToken.toString())
    for(spanLen <- 1 to n;
        iStart <- 0 until n - spanLen + 1) {
      val iEnd = iStart + spanLen - 1
      for (i <- iStart to iEnd) {
        totalCoveringsPerToken(i) += 1
        if (chart(iStart, iEnd).isEmpty)
          emptyCoveringsPerToken(i) += 1
      }
    }
    println("# empty coverings per token: " + emptyCoveringsPerToken.zipWithIndex.toList.toString)
    println("total   coverings per token: " + totalCoveringsPerToken.zipWithIndex.toList.toString)
    val percentCoveringsPerToken = scala.collection.mutable.ArrayBuffer.fill(n)(0.0)
    for (i <- 0 until n)
      percentCoveringsPerToken(i) = 1.0 * emptyCoveringsPerToken(i) / totalCoveringsPerToken(i)
    println("%       coverings per token: " + percentCoveringsPerToken.zipWithIndex.toList.toString)
  }

  /**
   * This algorithm determines whether each parseToken is "eventually recognized", i.e. whether it participates in some
   * partial parse.
   *
   * Suppose you have a chart C representing the parses of some sequence of parseTokens.
   * A span of the sequence is recognized if the chart C contains a parse for that span (a complete or partial parse).
   *
   * Then a parseToken in the original sequence is "eventually recognized" if it is a part of any span of parseTokens that
   * is recognized.
   *
   * This algorithm works by looking at the original parse chart, and creating a new chart of booleans indicating:
   *    For each span, will it "eventually" be recognized
   *    Will any span one larger that contains this span "eventually" be recognized
   *
   * The latter gives the essential recurrence relation.
   *
   * @param filterfn A function used to filter the chart entries. For example, we might want to strip Literals.
   */
  private[this] def findEventuallyRecognizedAs[T](chart: Chart[List[T]], filterfn: T => Boolean) = {
    val eventuallyRecognizedChart = new Chart[Boolean](new Array[String](chart.n))
    val n = chart.n
    for(spanLen <- n to 1 by -1) {
      for(iStart <- 0 to (n - spanLen)) {
        val iEnd = iStart + spanLen - 1
        eventuallyRecognizedChart(iStart, iEnd) = false
        val chartEntry = chart(iStart, iEnd).filter(filterfn)
        if (chartEntry.nonEmpty) {
          eventuallyRecognizedChart(iStart, iEnd) = true
        } else if (spanLen < n) {
          if (iStart > 0) {
            val expandLeft = eventuallyRecognizedChart(iStart - 1, iEnd)
            if (expandLeft) {
              eventuallyRecognizedChart(iStart, iEnd) = true
            }
          }
          if (iEnd < n - 1) {
            val expandRight = eventuallyRecognizedChart(iStart, iEnd + 1)
            if (expandRight) {
              eventuallyRecognizedChart(iStart, iEnd) = true
            }
          }
        }
      }
    }
    eventuallyRecognizedChart.diagonal
  }
}

package com.workday.montague.parser

import com.workday.montague.ccg.{CcgCat, CategoryParser}
import com.workday.montague.semantics.{Ignored, SemanticState}

import scala.collection.mutable
import scala.io.Source

case class ParserDict[S](map: Map[String, Seq[(S, SemanticState)]] = Map[String, Seq[(S, SemanticState)]](),
                         funcs: Seq[String => Seq[(S, SemanticState)]] = Nil,
                         fallbacks: Seq[String => Seq[(S, SemanticState)]] = Nil)
extends (String => Seq[(S, SemanticState)]) {

  def +[U](pair: U)(implicit adder: DictAdder[S, U]): ParserDict[S] = {
    adder(this, pair)
  }

  def withTerm(term: String, entries: Seq[(S, SemanticState)]): ParserDict[S] = {
    val updatedEntries = map.getOrElse(term, Seq()) ++ entries
    ParserDict(map.updated(term, updatedEntries), funcs, fallbacks)
  }

  def withTerms(termsAndEntries: Map[String, Seq[(S, SemanticState)]]): ParserDict[S] = {
    val newMap = mutable.Map(map.toSeq: _*)
    for ((term, entries) <- termsAndEntries) {
      val updatedEntries = map.getOrElse(term, Seq()) ++ entries
      newMap(term) = updatedEntries
    }
    ParserDict(newMap.toMap, funcs, fallbacks)
  }

  def withFunc(func: String => Seq[(S, SemanticState)]): ParserDict[S] = {
    ParserDict(map, funcs :+ func, fallbacks)
  }

  def withFallback(fallback: String => Seq[(S, SemanticState)]): ParserDict[S] = {
    ParserDict(map, funcs, fallbacks :+ fallback)
  }

  def apply(str: String): Seq[(S, SemanticState)] = {
    getMapEntries(str) ++ getFuncEntries(str) match {
      case Nil => getFallbackEntries(str)
      case entries => entries
    }
  }

  private def getMapEntries(str: String): Seq[(S, SemanticState)] = {
    for (entry <- map.getOrElse(str, Seq())) yield {
      entry
    }
  }

  private def getFuncEntries(str: String): Seq[(S, SemanticState)] = {
    for {
      func <- funcs
      entry <- func(str)
    } yield {
      entry
    }
  }

  private def getFallbackEntries(str: String): Seq[(S, SemanticState)] = {
    if (str contains " ") {
      Nil  // only fallback if str is a single token!
    } else {
      for {
        fallback <- fallbacks
        entry <- fallback(str)
      } yield {
        entry
      }
    }
  }
}

object ParserDict {
  def fromCcgBankLexicon(path: String): ParserDict[CcgCat] = fromSpaceSeparatedLexicon(path, 0, 4)

  /**
   * The old-style CCG Bank lexicons (http://juliahmr.cs.illinois.edu/CCGlexicon/) had a slightly different
   * column ordering.
   */
  def fromOldCcgBankLexicon(path: String): ParserDict[CcgCat] = fromSpaceSeparatedLexicon(path, 0, 3)

  private def fromSpaceSeparatedLexicon(path: String, termIdx: Int, categoryProbIdx: Int): ParserDict[CcgCat] = {
    val lexiconMap: mutable.Map[String, mutable.ListBuffer[CcgCat]] = mutable.Map()

    val file = Source.fromFile(path)
    for (line <- file.getLines()) {
      val parts = line.split("[\t ]+")
      val term = parts(termIdx)
      val parsedCategory: CategoryParser.ParseResult[CcgCat] = CategoryParser(parts(1))
      val categoryProbability = parts(categoryProbIdx).toDouble
      if (parsedCategory.successful) {
        val cat: CcgCat = parsedCategory.get % categoryProbability
        if (lexiconMap contains term) {
          lexiconMap(term).append(cat)
        } else {
          lexiconMap(term) = mutable.ListBuffer(cat)
        }
      }
    }
    ParserDict[CcgCat](syntaxToSemantics(lexiconMap.toMap.mapValues(s => s.toSeq)))
  }

  private def syntaxToSemantics[S](inputMap: Map[String, Seq[S]]): Map[String, Seq[(S, SemanticState)]] = {
    for ((term, entries) <- inputMap) yield {
      term -> entries.map(_ -> Ignored(term))
    }
  }
}

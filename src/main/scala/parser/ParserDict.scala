package parser

import ccg.{CcgCat, CategoryParser}
import semantics.{Ignored, SemanticState}

import scala.collection.mutable
import scala.io.Source

trait ParserDict[S] extends (String => Seq[(S, SemanticState)]) {
  def combine(other: ParserDict[S]): ParserDict[S] = {
    ChainedDict(this, other)
  }

  def withMatcher(matcher: String => Seq[(S, SemanticState)]): ParserDict[S] = {
    ChainedDict(this, MatcherDict(matcher))
  }
}

case class MatcherDict[S](matcher: String => Seq[(S, SemanticState)]) extends ParserDict[S] {
  def apply(str: String): Seq[(S, SemanticState)] = {
    matcher(str)
  }
}

case class MapDict[S](map: Map[String, Seq[(S, SemanticState)]]) extends ParserDict[S] {
  def apply(str: String): Seq[(S, SemanticState)] = {
    for (entry <- map.getOrElse(str, Seq())) yield {
      entry
    }
  }
}

case class ChainedDict[S](dict1: ParserDict[S], dict2: ParserDict[S]) extends ParserDict[S] {
  def apply(str: String): Seq[(S, SemanticState)] = {
    dict1(str) ++ dict2(str)
  }
}

object ParserDict {
  def fromMap[S](map: Map[String, Seq[(S, SemanticState)]]): ParserDict[S] = {
    MapDict[S](map)
  }

  def fromMultiMap[S](map: Map[Seq[String], Seq[(S, SemanticState)]]): ParserDict[S] = {
    /**
     * Convert a map from Seq[A] -> Seq[B], to A -> Seq[B], flattening out the A's and concatenating any
     * duplicate entries of an A
     */
    def multikeymap[A, B](keymap: Map[Seq[A], Seq[B]]): Map[A, Seq[B]] = {
      val map = mutable.LinkedHashMap[A, Seq[B]]()
      for {
        (keys, values) <- keymap
        key <- keys
      } {
        if (map.contains(key)) {
          map(key) ++= values
        } else {
          map(key) = values
        }
      }
      map.toMap
    }

    fromMap(multikeymap[String, (S, SemanticState)](map))
  }

  def fromCcgBankLexicon(path: String): ParserDict[CcgCat] = {
    // @todo: Incorporate the probabilities of each ccg category for each term

    val lexiconMap: mutable.Map[String, mutable.ListBuffer[CcgCat]] = mutable.Map()

    val file = Source.fromFile(path)
    for (line <- file.getLines()) {
      val parts = line.split(" +")
      val term = parts(0)
      val parsedCategory: CategoryParser.ParseResult[CcgCat] = CategoryParser(parts(1))
      val prob = parts(4).toDouble
      if (parsedCategory.successful) {
        val cat: CcgCat = parsedCategory.get % prob
        if (lexiconMap contains term) {
          lexiconMap(term).append(cat)
        } else {
          lexiconMap(term) = mutable.ListBuffer(cat)
        }
      }
    }

    MapDict[CcgCat](WithDummySemantics(lexiconMap.toMap.mapValues(s => s.toSeq)))
  }
}

object WithDummySemantics {
  def apply[S](syntaxMap: Map[String, Seq[S]]): Map[String, Seq[(S, SemanticState)]] = {
    for ((term, entries) <- syntaxMap) yield {
      term -> entries.map(_ -> Ignored(term))
    }
  }
}
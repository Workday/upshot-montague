package parser

import ccg.{CcgCat, CategoryParser}
import semantics.{Ignored, SemanticState}

import scala.collection.mutable
import scala.io.Source

case class ParserDict[S](map: Map[String, Seq[(S, SemanticState)]] = Map[String, Seq[(S, SemanticState)]](),
                         funcs: Seq[String => Seq[(S, SemanticState)]] = Nil,
                         fallbacks: Seq[String => (S, SemanticState)] = Nil)
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

  def withFunc(func: String => Seq[(S, SemanticState)]) = {
    ParserDict(map, funcs :+ func, fallbacks)
  }

  def withFallback(fallback: String => (S, SemanticState)) = {
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
      for (fallback <- fallbacks) yield {
        fallback(str)
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

trait DictAdder[S, U] {
  def apply(dict: ParserDict[S], pair: U): ParserDict[S]
}

object DictAdder {
  implicit def stringSyntaxAdder[S, T <: S] = new DictAdder[S, (String, T)] {
    // e.g. dict + (term -> category)
    def apply(dict: ParserDict[S], pair: (String, T)) = {
      val term = pair._1
      val syntax = pair._2
      dict.withTerm(term, Seq((syntax, Ignored(term))))
    }
  }

  implicit def stringSyntaxSemanticsAdder[S, T <: S] = new DictAdder[S, (String, (T, SemanticState))] {
    // e.g. dict + (term -> (category, semantics))
    def apply(dict: ParserDict[S], pair: (String, (T, SemanticState))) = {
      val term = pair._1
      val syntax = pair._2._1
      val semantics = pair._2._2
      dict.withTerm(term, Seq((syntax, semantics)))
    }
  }

  implicit def stringSeqSyntaxAdder[S, T <: S] = new DictAdder[S, (String, Seq[T])] {
    // e.g. dict + (term -> Seq(category1, category2, ...))
    def apply(dict: ParserDict[S], pair: (String, Seq[T])) = {
      val term = pair._1
      val syntax = pair._2
      dict.withTerm(term, syntax.map(s => s -> Ignored(term)))
    }
  }

  implicit def stringSeqSyntaxSemanticsAdder[S, T <: S] = new DictAdder[S, (String, Seq[(T, SemanticState)])] {
    // e.g. dict + (term -> Seq((category1, semantics1), (category2, semantics2), ...))
    def apply(dict: ParserDict[S], pair: (String, Seq[(T, SemanticState)])) = {
      val term = pair._1
      val entries = pair._2
      dict.withTerm(term, entries)
    }
  }

  implicit def seqStringSyntaxAdder[S, T <: S] = new DictAdder[S, (Seq[String], T)] {
    // e.g. dict + (Seq(synonym1, synonym2, ...) -> category)
    def apply(dict: ParserDict[S], pair: (Seq[String], T)) = {
      val terms = pair._1
      val syntax = pair._2
      dict.withTerms(terms.map(t => t -> Seq((syntax, Ignored(t)))) toMap)
    }
  }

  implicit def seqStringSyntaxSemanticsAdder[S, T <: S] = new DictAdder[S, (Seq[String], (T, SemanticState))] {
    // e.g. dict + (Seq(synonym1, synonym2, ...) -> (category, meaning))
    def apply(dict: ParserDict[S], pair: (Seq[String], (T, SemanticState))) = {
      val terms = pair._1
      val syntax = pair._2._1
      val semantics = pair._2._2
      dict.withTerms(terms.map(t => t -> Seq((syntax, semantics))) toMap)
    }
  }

  implicit def seqStringSeqSyntaxAdder[S, T <: S] = new DictAdder[S, (Seq[String], Seq[T])] {
    // e.g. dict + (Seq(synonym1, synonym2, ...) -> Seq(category1, category2, ...))
    def apply(dict: ParserDict[S], pair: (Seq[String], Seq[T])) = {
      val terms = pair._1
      val syntax = pair._2
      dict.withTerms(terms.map(t => t -> syntax.map(s => s -> Ignored(t))) toMap)
    }
  }

  implicit def seqStringSeqSyntaxSemanticsAdder[S, T <: S] = new DictAdder[S, (Seq[String], Seq[(T, SemanticState)])] {
    // e.g. dict + (Seq(synonym1, synonym2, ...) -> Seq((category1, semantics1), (category2, semantics2), ...))
    def apply(dict: ParserDict[S], pair: (Seq[String], Seq[(T, SemanticState)])) = {
      val terms = pair._1
      val entries = pair._2
      dict.withTerms(terms.map(t => t -> entries) toMap)
    }
  }

  implicit def matcherSemanticsAdder[S, T <: S, V, W <: String => Seq[V], Y <: SemanticState] = new DictAdder[S, (W, (T, V => Y))] {
    // e.g. dict + (matcherFunc -> (category, {matchedTerm => semantics}))
    def apply(dict: ParserDict[S], pair: (W, (T, V => Y))) = {
      val matcher = pair._1
      val syntax = pair._2._1
      val semantics = pair._2._2
      val func = (str: String) => matcher(str).map(m => (syntax, semantics(m)))
      dict.withFunc(func)
    }
  }

  implicit def matcherSyntaxSemanticsAdder[S, T <: S, V, W <: String => Seq[V], Y <: SemanticState] = new DictAdder[S, (W, (V => T, V => Y))] {
    // e.g. dict + (matcherFunc -> ({matchedTerm => category}, {matchedTerm => semantics}))
    def apply(dict: ParserDict[S], pair: (W, (V => T, V => Y))) = {
      val matcher = pair._1
      val syntax = pair._2._1
      val semantics = pair._2._2
      val func = (str: String) => matcher(str).map(m => (syntax(m), semantics(m)))
      dict.withFunc(func)
    }
  }

  implicit def elseSyntaxAdder[S, T <: S] = new DictAdder[S, (Else.type, T)] {
    // e.g. dict + (Else -> category)
    def apply(dict: ParserDict[S], pair: (Else.type, T)) = {
      val syntax = pair._2
      val func = (str: String) => (syntax, Ignored(str))
      dict.withFallback(func)
    }
  }

  implicit def elseSyntaxSemanticsAdder[S, T <: S, Y <: SemanticState] = new DictAdder[S, (Else.type, (T, String => Y))] {
    // e.g. dict + (Else -> (category, {matchedTerm => semantics}))
    def apply(dict: ParserDict[S], pair: (Else.type, (T, String => Y))) = {
      val syntax = pair._2._1
      val semantics = pair._2._2
      val func = (str: String) => (syntax, semantics(str))
      dict.withFallback(func)
    }
  }
}

object Else

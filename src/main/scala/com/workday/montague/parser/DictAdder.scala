package com.workday.montague.parser

import com.workday.montague.semantics._

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
      val func = (str: String) => Seq(syntax -> Ignored(str))
      dict.withFallback(func)
    }
  }

  implicit def elseSyntaxSemanticsAdder[S, T <: S, Y <: SemanticState] = new DictAdder[S, (Else.type, (T, String => Y))] {
    // e.g. dict + (Else -> (category, {matchedTerm => semantics}))
    def apply(dict: ParserDict[S], pair: (Else.type, (T, String => Y))) = {
      val syntax = pair._2._1
      val semantics = pair._2._2
      val func = (str: String) => Seq(syntax -> semantics(str))
      dict.withFallback(func)
    }
  }

  implicit def elseSeqSyntaxAdder[S, T <: S] = new DictAdder[S, (Else.type, Seq[T])] {
    // e.g. dict + (Else -> category)
    def apply(dict: ParserDict[S], pair: (Else.type, Seq[T])) = {
      val syntax = pair._2
      val func = (str: String) => syntax.map(s => s -> Ignored(str))
      dict.withFallback(func)
    }
  }
}

trait DictAdder[S, U] {
  def apply(dict: ParserDict[S], pair: U): ParserDict[S]
}

object Else

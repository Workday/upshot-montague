package example

import ccg._
import parser.{Else, ParserDict, SemanticParser}
import semantics._

sealed trait Statement
case class Define(relation: String, subject: String, predicate: String)
case class Query(relation: String, subject: String)

case object Q extends TerminalCat { val category = "Q" } // syntactic category for questions

object InformationStore extends SemanticParser[CcgCat](InformationStoreLexicon.lexicon)

object InformationStoreLexicon {
  val lexicon = ParserDict.fromOldCcgBankLexicon("data/lexicon.wsj02-21") +
    ("is" -> Seq(
      ((S\NP)/NP, λ {pred: String => λ {subject: String => Define("IS", subject, pred)}}),
      ((S\Q)/NP, λ {subject: String => λ {question: String => Query("IS", subject)}})
    )) +
    ("who" -> Q) +
    (Else -> Seq(N % 0.9, X % 0.1))  // unrecognized terms are probably nouns, but could be anything
}

package example

import ccg._
import parser.{SemanticRepl, Else, ParserDict, SemanticParser}
import semantics._

object InformationStore extends SemanticRepl[CcgCat, Statement, Seq[Define]](InformationStoreParser) {
  def performAction(store: Seq[Define], action: Statement): Seq[Define] = {
    action match {
      case definition@Define(_, _, _) =>
        println("Ok")
        store :+ definition
      case Query(relation, subject) =>
        val candidates = store.filter(d => d.relation == relation && d.subject == subject)
                              .map(_.predicate)
        println(candidates.length match {
          case 0 => "I don't know"
          case 1 => candidates.head
          case _ => candidates.mkString("{", ", ", "}")
        })
        store
    }
  }

  def onParseError(line: String): Unit = println("I can't parse that")

  val initialModel: Seq[Define] = Seq()
}

object InformationStoreParser extends SemanticParser[CcgCat](InformationStoreLexicon.lexicon)

// Semantics
sealed trait Statement
case class Define(relation: String, subject: String, predicate: String) extends Statement
case class Query(relation: String, subject: String) extends Statement

// Syntax
case object Q extends TerminalCat { val category = "Q" } // syntactic category for question words

// Lexicon
object InformationStoreLexicon {
  val lexicon = ParserDict.fromOldCcgBankLexicon("data/lexicon.wsj02-21") +
    (Seq("is", "are") -> relation("BE")) +
    (Seq("who", "what", "how", "where", "when") -> Q) +
    (Else -> Seq(N % 0.8, N/N % 0.1, X % 0.1))

  private def relation(relationType: String) = {
    Seq(
      // e.g. "Checkers is a dog"
      ((S\NP)/NP, λ {pred: String => λ {subject: String => Define(relationType, subject, pred)}}),
      // e.g. "Checkers is fluffy"
      ((S\NP)/(N/N), λ {pred: String => λ {subject: String => Define(relationType, subject, pred)}}),
      // e.g. "Who is Checkers?"
      ((S\Q)/NP, λ {subject: String => λ {question: String => Query(relationType, subject)}})
    )
  }
}

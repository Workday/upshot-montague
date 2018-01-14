package example

import com.workday.montague.ccg.{TerminalCat, X, N, CcgCat}
import com.workday.montague.parser._
import com.workday.montague.semantics._

case object Paren extends TerminalCat { val category = "Paren" } // syntactic category for parenthetical expressions

object ArithmeticParser extends SemanticParser[CcgCat](ArithmeticLexicon.lexicon) {
  // We need a custom tokenizer to separate parentheses from adjoining terms
  override val tokenizer: String => IndexedSeq[String] =
    str => str.replace("(", " ( ").replace(")", " ) ").trim.toLowerCase.split("\\s+")
}

object ArithmeticLexicon {
  val lexicon =  ParserDict[CcgCat]() +
    (Seq("+", "plus") -> ((N\N)/N, λ {y: Int => λ {x: Int => x + y}})) +
    (Seq("-", "minus") -> ((N\N)/N, λ {y: Int => λ {x: Int => x - y}})) +
    (Seq("*", "times") -> ((N\N)/N, λ {y: Int => λ {x: Int => x * y}})) +
    (Seq("+/-", "plus/minus") -> Seq( // example of ambiguous definition
      ((N\N)/N, λ {y: Int => λ {x: Int => x + y}}),
      ((N\N)/N, λ {y: Int => λ {x: Int => x - y}})
    )) +
    ("(" -> (Paren/N, identity)) +
    (")" -> (N\Paren, identity)) +
    (IntegerMatcher -> (N, {i: Int => Form(i)})) +  // IntegerMatcher matches using Integer.parseInt
    (Else -> (X|X, {s: String => identity}))  // Else matches any single tokens that don't match anything else;
                                              // X|X is the identity CCG category
}

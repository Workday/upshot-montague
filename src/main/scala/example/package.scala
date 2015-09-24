import ccg._
import parser._
import semantics._

package object example {
  case object Paren extends TerminalCat { val category = "Paren" } // syntactic category for parenthetical expressions

  val arithmeticLexicon = ParserDict[CcgCat]() +
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
    (Else -> (X|X, {s: String => identity}))  // X|X is the identity CCG category
}

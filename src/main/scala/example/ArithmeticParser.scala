package example

import ccg.{X, N, CcgCat}
import parser._
import semantics._

object ArithmeticParser extends SemanticParser[CcgCat](ArithmeticLexicon.lexicon) {
  def parse(str: String): SemanticParseResult[CcgCat] = parse(str, tokenizer = parenTokenizer)

  def main(args: Array[String]): Unit = {
    val input = args.mkString(" ")
    val result = parse(input)
    val output = result.semantics

    println(s"Input: $input")
    println(s"Output: $output")

    if (result.bestParse.isDefined) {
      // Print out the best parse in Graphviz Dot format
      // println(result.bestParse.get.toDotString)
    }
  }

  // We need a custom tokenizer to separate parentheses from adjoining terms
  private def parenTokenizer(str: String) = {
    str.replace("(", " ( ").replace(")", " ) ").trim.toLowerCase.split("\\s+")
  }
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

package example

import ccg.CcgCat
import parser.SemanticParser

object ArithmeticParser extends SemanticParser[CcgCat](arithmeticLexicon) {
  def main(args: Array[String]): Unit = {
    val input = args.mkString(" ")
    val result = parse(input, tokenizer = parenTokenizer)
    val output = result.semantics

    println(s"Input: $input")
    println(s"Output: $output")
  }

  // We need a custom tokenizer to separate parentheses from adjoining terms
  private def parenTokenizer(str: String) = {
    str.replace("(", " ( ").replace(")", " ) ").trim.toLowerCase.split("\\s+")
  }
}

package example

import ccg.CcgCat
import parser.{SemanticParseResult, SemanticParser}

object ArithmeticParser extends SemanticParser[CcgCat](arithmeticLexicon) {
  def parse(str: String): SemanticParseResult[CcgCat] = parse(str, tokenizer = parenTokenizer)

  def main(args: Array[String]): Unit = {
    val input = args.mkString(" ")
    val result = parse(input)
    val output = result.semantics

    println(s"Input: $input")
    println(s"Output: $output")
  }

  // We need a custom tokenizer to separate parentheses from adjoining terms
  private def parenTokenizer(str: String) = {
    str.replace("(", " ( ").replace(")", " ) ").trim.toLowerCase.split("\\s+")
  }
}

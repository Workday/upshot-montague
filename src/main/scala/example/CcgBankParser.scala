package example

import ccg.{CcgCat, N, X}
import parser.{Else, ParserDict, SemanticParser}
import semantics.Ignored

object CcgBankParser extends SemanticParser[CcgCat](CcgBankLexicon.lexicon) {
  def main(args: Array[String]): Unit = {
    val input = args.mkString(" ")
    val result = parse(input)
    val output = result.bestParse.map(_.semantic) match {
      case Some(Ignored(parse)) => parse
      case _ => "(failed to parse)"
    }

    println(s"Input: $input")
    println(s"Highest-scoring parse: $output")

    if (result.bestParse.isDefined) {
      // Print out the best parse in Graphviz Dot format
      // println(result.bestParse.get.toDotString)
    }
  }
}

object CcgBankLexicon {
  val lexicon = ParserDict.fromOldCcgBankLexicon("data/lexicon.wsj02-21") +
    (Else -> Seq(N % 0.9, X % 0.1))  // unrecognized terms are probably nouns, but could be anything
}

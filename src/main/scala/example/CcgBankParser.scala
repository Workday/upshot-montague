package example

import ccg.{NP, N, X, CcgCat}
import parser.{Else, SemanticParser, ParserDict}
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
    println(s"Parse: $output")
    println(s"Chart: ${result.bestParse.map(_.toStringHelp(withSemantics = false))}")
  }
}

object CcgBankLexicon {
  val lexicon = ParserDict.fromOldCcgBankLexicon("data/lexicon.wsj02-21") +
    (Else -> Seq(NP % 0.45, N % 0.45, X % 0.1))  // unrecognized terms are probably nouns, but could be anything
}

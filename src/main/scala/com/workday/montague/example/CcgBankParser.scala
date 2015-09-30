package example

import com.workday.montague.ccg.{CcgCat, N, X}
import com.workday.montague.parser.{Else, ParserDict, SemanticParser}

object CcgBankParser extends SemanticParser[CcgCat](CcgBankLexicon.lexicon)

object CcgBankLexicon {
  val lexicon = ParserDict.fromOldCcgBankLexicon("data/CCGbank.00-24.lexicon") +
    (Else -> Seq(N % 0.9, X % 0.1))  // unrecognized terms are probably nouns, but could be anything
}

object OldCcgBankParser extends SemanticParser[CcgCat](OldCcgBankLexicon.lexicon)

object OldCcgBankLexicon {
  val lexicon = ParserDict.fromOldCcgBankLexicon("data/lexicon.wsj02-21") +
    (Else -> Seq(N % 0.9, X % 0.1))  // unrecognized terms are probably nouns, but could be anything
}

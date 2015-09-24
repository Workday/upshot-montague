package parser

import ccg._
import org.scalatest.FlatSpec
import semantics._

class SemanticParserSpec extends FlatSpec {
  it should "perform a syntactic parse with a simple dictionary" in {
    val lexicon = ParserDict[CcgCat]() +
      ("the" -> NP/N) +
      ("quick" -> (N|N)) +
      ("brown" -> (N|N)) +
      ("ox" -> N) +
      ("and" -> conj) +
      ("silly" -> (N|N)) +
      ("cat" -> N) +
      ("jump" -> (S\NP)/PP) +
      ("over" -> PP/NP) +
      ("lazy" -> (N|N)) +
      ("dog" -> N)

    val parser = new SemanticParser[CcgCat](lexicon)

    val result = parser.parse("the quick brown ox and the silly cat jump over the lazy dog")
    assert(result.bestParse.isDefined)

    val parse: SemanticParseNode[CcgCat] = result.bestParse.get

    // The lexicon above is syntactic only (no semantics), so the "semantic" state outputted is
    // Ignored(string representation of the parse). We check to make sure we parsed as expected.
    assert(parse.semantic == Ignored("jump(over(the(lazy(dog))))(and(the(silly(cat)))(the(quick(brown(ox)))))"))
  }

  it should "perform a syntactic parse with a lexicon loaded from CCGbank" in {
    // Load a (very) abridged CCGbank lexicon, with just the words we need to parse our example sentence
    val ccgBankLexicon = ParserDict.fromCcgBankLexicon("src/test/resources/CCGbank.00-24.lexicon.partial")
    val parser = new SemanticParser[CcgCat](ccgBankLexicon)

    val result = parser.parse("the quick brown ox and the silly cat jump over the lazy dog")

    // Because there are so many different entries for each of these words in our test CCGbank lexicon,
    // we expect > 100 different parses
    assert(result.parses.size > 100)

    val bestParse: SemanticParseNode[CcgCat] = result.bestParse.get
    val worstParse: SemanticParseNode[CcgCat] = result.parses.sortBy(node => node.syntactic.score).head

    // Make sure we get what we expect for the highest-scoring ("best") parse
    assert(bestParse.semantic == Ignored("jump(over(the(lazy(dog))))(and(the(silly(cat)))(the(quick(brown(ox)))))"))
    // and get something different for other parses, such as the lowest-scoring ("worst") parse
    assert(worstParse.semantic != Ignored("jump(over(the(lazy(dog))))(and(the(silly(cat)))(the(quick(brown(ox)))))"))
  }

  it should "perform a semantic parse with a simple mathematical dictionary" in {
    case object Paren extends TerminalCat { val category = "Paren" } // syntactic category for parenthetical expressions

    val mathLexicon = ParserDict[CcgCat]() +
      ("plus" -> ((N\N)/N, λ {y: Int => λ {x: Int => x + y}})) +
      ("minus" -> ((N\N)/N, λ {y: Int => λ {x: Int => x - y}})) +
      ("times" -> ((N\N)/N, λ {y: Int => λ {x: Int => x * y}})) +
      ("plus/minus" -> Seq( // example of ambiguous definition
        ((N\N)/N, λ {y: Int => λ {x: Int => x + y}}),
        ((N\N)/N, λ {y: Int => λ {x: Int => x - y}})
      )) +
      ("(" -> (Paren/N, identity)) +
      (")" -> (N\Paren, identity)) +
      (Seq("what is", "?") -> (X|X, identity)) + // X|X is the identity CCG category
      (IntegerMatcher -> (N, {i: Int => Form(i)}))  // IntegerMatcher matches using Integer.parseInt

    // We need a custom tokenizer to separate parentheses from adjoining terms
    def parenTokenizer(str: String) = {
      str.replace("(", " ( ").replace(")", " ) ").trim.toLowerCase.split("\\s+")
    }

    val parser = new SemanticParser[CcgCat](mathLexicon)

    val result = parser.parse("What is (2 plus 3) times (8 plus/minus 4)?", parenTokenizer)

    // 2+3 * (8±4) = 60 or 20
    assert(result.semantics == Ambiguous(Set(Form(60), Form(20))))
  }
}

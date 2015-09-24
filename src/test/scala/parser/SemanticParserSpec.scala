package parser

import ccg._
import org.scalatest.FlatSpec
import semantics._

class SemanticParserSpec extends FlatSpec {
  it should "perform a syntactic parse with a simple dictionary" in {
    val localDict = ParserDict[CcgCat]() +
      ("the" -> NP/N) +
      ("quick" -> (N|N)) +
      ("brown" -> (N|N)) +
      ("ox" -> N) +
      ("and" -> conj) +
      ("silly" -> (N|N)) +
      ("cat" -> N) +
      ("jump" -> (S\NP)/PP) +
      ("over" -> PP/NP) +
      ("a" -> NP/N) +
      ("lazy" -> (N|N)) +
      ("dog" -> N)

    val parser = new SemanticParser[CcgCat](localDict)

    val result = parser.parse("the quick brown ox and the silly cat jump over the lazy dog")
    assert(result.bestParse.isDefined)

    val parse: SemanticParseNode[CcgCat] = result.bestParse.get

    // The lexicon above is syntactic only (no semantics), so the "semantic" state outputted is
    // Ignored(string representation of the parse). We check to make sure we parsed as expected.
    assert(parse.semantic == Ignored("jump(over(the(lazy(dog))))(and(the(silly(cat)))(the(quick(brown(ox)))))"))
  }

  it should "perform a semantic parse with a simple mathematical dictionary" in {
    case object Paren extends TerminalCat { val category = "Paren" } // syntactic category for parenthetical expressions

    val mathDict = ParserDict[CcgCat]() +
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

    val parser = new SemanticParser[CcgCat](mathDict)

    val result = parser.parse("What is (2 plus 3) times (8 plus/minus 4)?", parenTokenizer)

    // 2+3 * (8±4) = 60 or 20
    assert(result.semantics == Ambiguous(Set(Form(60), Form(20))))
  }
}

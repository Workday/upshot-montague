package parser

import ccg._
import cky._
import semantics._

import scala.collection.{mutable, IterableLike}
import scala.collection.generic.CanBuildFrom

class SemanticParser[S <: SyntacticLabel[S]](dict: ParserDict[S],
                                             override protected val timeLimitSecs: Double = 10.0) extends CkyParserWithList[SemanticParseNode[S]] {
  protected type Node = SemanticParseNode[S]

  def parse(str: String, tokenizer: String => IndexedSeq[String] = defaultTokenizer): SemanticParseResult[S] = {
    parse(tokenizer(str))
  }

  def parse(tokens: IndexedSeq[String]): SemanticParseResult[S] = {
    val chart = super.parseToChart(tokens)
    new SemanticParseResult(tokens, chart)
  }

  private def defaultTokenizer(str: String): IndexedSeq[String] = {
    str.trim.toLowerCase.split("\\s+")
  }
  
  override protected def dictLookup(parseToken: ParseToken, spans: Spans): List[Node] = {
    (for (entry <- dict(parseToken.tokenString))
      yield Terminal(entry._1, entry._2, parseToken, spans)
    ).toList
  }

  override protected def derive(left: Node, right: Node): List[Node] = {
    getAllDerivations(left, right).toList
  }

  private[this] def getAllDerivations(left: Node, right: Node): List[Node] = {
    deriveRightward(left, right) ++ deriveLeftward(left, right)
  }

  private[this] def deriveRightward(left: Node, right: Node): List[Node] = {
    val derivations = left.syntactic.deriveRightward(right.syntactic)
    derivations.flatMap(createDerivedNode(left, right, _))
  }

  private[this] def deriveLeftward(left: Node, right: Node): List[Node] = {
    val derivations = right.syntactic.deriveLeftward(left.syntactic)
    derivations.flatMap(createDerivedNode(right, left, _))
  }

  private[this] def createDerivedNode(predNode: Node, argNode: Node, newSyntactic: S): Option[NonTerminal[S]] = {
    val newSemantic = predNode.semantic.apply(argNode.semantic)
    newSemantic match {
      case Nonsense => None
      case _ => Some(NonTerminal(newSyntactic, newSemantic, predNode, argNode))
    }
  }

  /**
   * Remove invalid parses
   * And deduplicate
   */
  override protected def postStep(matrix: Chart[List[Node]], tokens: IndexedSeq[String], iStart: Int, iEnd: Int): Unit = {
    super.postStep(matrix, tokens, iStart, iEnd)
    if (matrix(iStart, iEnd).nonEmpty) {
      matrix(iStart, iEnd) =
        matrix(iStart, iEnd)
          .filter(_.semantic != Nonsense)
          .distinctBy(x => (x.semantic, x.syntactic))
    }
  }

  // http://stackoverflow.com/questions/3912753/scala-remove-duplicates-in-list-of-objects
  implicit class RichCollection[A, Repr](xs: IterableLike[A, Repr]){
    def distinctBy[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, A, That]) = {
      val builder = cbf(xs.repr)
      val i = xs.iterator
      var set = Set[B]()
      while(i.hasNext) {
        val o = i.next()
        val b = f(o)
        if (!set(b)) {
          set += b
          builder += o
        }
      }
      builder.result()
    }
  }
}

object SemanticParser { 
  def main(args: Array[String]) {
    /*val localDict = ParserDict.fromMap(
      WithDummySemantics(
        Map(
          "the" -> Seq(NP/N),
          "quick" -> Seq(N|N),
          "brown" -> Seq(N|N),
          "ox" -> Seq(N),
          "and" -> Seq(conj),
          "silly" -> Seq(N|N),
          "cat" -> Seq(N),
          "jump" -> Seq((S\NP)/PP),
          "over" -> Seq(PP/NP),
          "a" -> Seq(NP/N),
          "lazy" -> Seq(N|N),
          "dog" -> Seq(N)
        )
      )
    )
    val ccgBankDict = ParserDict.fromCcgBankLexicon("CCGbank.00-24.lexicon")

    //val parser = new SemanticParser[CcgCat](localDict)
    val parser = new SemanticParser[CcgCat](ccgBankDict)

    val result = parser.parse("the quick brown ox and the silly cat jump over the lazy dog")*/

    val mathDict = ParserDict.fromMap(
      Map(
        "plus" -> Seq(((N\N)/N, lift2[Integer]({ case x => {case y => x + y}}))),
        "minus" -> Seq(((N\N)/N, lift2[Integer]({ case x => {case y => x - y}}))),
        "times" -> Seq(((N\N)/N, lift2[Integer]({ case x => {case y => x * y}}))),
        "(" -> Seq((NP/N, identity)),
        ")" -> Seq((N\NP, identity)),
        "what is" -> Seq((IdentityCat, identity)),
        "?" -> Seq((IdentityCat, identity))
      )
    ).withMatcher(IntegerMatcher(i => i))

    def parenTokenizer(str: String) = {
      str.replace("(", " ( ").replace(")", " ) ").trim.toLowerCase.split("\\s+")
    }

    val parser = new SemanticParser[CcgCat](mathDict)
    val result = parser.parse("What is (2 plus 3) times (4 plus 5)?", parenTokenizer)

    println(result.bestParse)
    result.debugPrint()
  }
}
package parser

import ccg.SyntacticLabel
import cky._
import semantics._

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom

trait SemanticParser[S <: SyntacticLabel[S]] extends CkyParserWithList[SemanticParseNode[S]] {
  protected type Node = SemanticParseNode[S]

  def parse(tokens: IndexedSeq[String]) = {
    val chart = super.parseToChart(tokens)
    new SemanticParseResult(tokens, chart)
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

  override protected def derive(left: SemanticParseNode[S], right: SemanticParseNode[S]): List[SemanticParseNode[S]] = {
    getAllDerivations(left, right).toList
  }

  override protected def concat(a: List[SemanticParseNode[S]], b: List[SemanticParseNode[S]]) = super.concat(a, b)

  private[this] def getAllDerivations(left: SemanticParseNode[S], right: SemanticParseNode[S]): List[SemanticParseNode[S]] = {
    deriveRightward(left, right) ++ deriveLeftward(left, right)
  }

  private[this] def deriveRightward(left: SemanticParseNode[S], right: SemanticParseNode[S]): List[SemanticParseNode[S]] = {
    val derivations = left.syntactic.deriveRightward(right.syntactic)
    derivations.flatMap(createDerivedNode(left, right, _))
  }

  private[this] def deriveLeftward(left: SemanticParseNode[S], right: SemanticParseNode[S]): List[SemanticParseNode[S]] = {
    val derivations = right.syntactic.deriveLeftward(left.syntactic)
    derivations.flatMap(createDerivedNode(right, left, _))
  }

  private[this] def createDerivedNode(predNode: SemanticParseNode[S], argNode: SemanticParseNode[S], newSyntactic: S): Option[NonTerminal[S]] = {
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
  override protected def postStep(matrix: Chart[List[SemanticParseNode[S]]], tokens: IndexedSeq[String], iStart: Int, iEnd: Int): Unit = {
    super.postStep(matrix, tokens, iStart, iEnd)
    if (matrix(iStart, iEnd).nonEmpty) {
      matrix(iStart, iEnd) =
        matrix(iStart, iEnd)
          .filter(_.semantic != Nonsense)
          .distinctBy(x => (x.semantic, x.syntactic))
    }
  }
}
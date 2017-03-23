package com.workday.montague.parser

import com.workday.montague.ccg._
import com.workday.montague.cky._
import com.workday.montague.semantics._

import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom

class SemanticParser[S <: SyntacticLabel[S]](dict: ParserDict[S],
                                             override protected val timeLimitSecs: Double = 10.0)
extends CkyParserWithList[SemanticParseNode[S]] {

  protected type Node = SemanticParseNode[S]

  def main(args: Array[String]): Unit = {
    val input = args.mkString(" ")
    val result = parse(input)
    val output = result.bestParse.map(_.semantic.toString).getOrElse("(failed to parse)")

    println(s"Input: $input")
    println(s"Output: $output")

    // Print out the full syntactic parse tree of the best parse
    // println(result.bestParse)

    // Print out the best parse in Graphviz Dot format
    // println(result.bestParse.get.toDotString)

    // Print out the best parse in ASCII format
    // println(result.bestParse.get.toString)
  }

  def parse(str: String, tokenizer: String => IndexedSeq[String] = defaultTokenizer): SemanticParseResult[S] = {
    parse(tokenizer(str))
  }

  def parse(tokens: IndexedSeq[String]): SemanticParseResult[S] = {
    val chart = super.parseToChart(tokens)
    new SemanticParseResult(tokens, chart)
  }

  private def defaultTokenizer(str: String): IndexedSeq[String] = {
    str.trim.toLowerCase.split("\\s+|[.?!]")
  }

  override protected def dictLookup(parseToken: ParseToken, spans: Spans): List[Node] = {
    (for (entry <- dict(parseToken.tokenString))
      yield Terminal(entry._1, entry._2, parseToken, spans, Set())
    ).toList
  }

  override protected def derive(left: Node, right: Node): List[Node] = {
    getAllDerivations(left, right)
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
      case Nonsense(exs) => Some(NonTerminal(newSyntactic, newSemantic, predNode, argNode, predNode.exs ++ argNode.exs ++ exs))
      case _ => Some(NonTerminal(newSyntactic, newSemantic, predNode, argNode, predNode.exs ++ argNode.exs))
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
          // .filter(!_.semantic.isInstanceOf[Nonsense])
          .sortBy(_.exs.size)
          .distinctBy(x => (x.semantic, x.syntactic))
    }
  }

  // http://stackoverflow.com/questions/3912753/scala-remove-duplicates-in-list-of-objects
  implicit class RichCollection[A, Repr](xs: IterableLike[A, Repr]){
    def distinctBy[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, A, That]): That = {
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

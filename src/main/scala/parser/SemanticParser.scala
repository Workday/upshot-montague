package parser

import ccg._
import cky._
import semantics._

import scala.io.Source
import scala.collection.{mutable, IterableLike}
import scala.collection.generic.CanBuildFrom

class SemanticParser[S <: SyntacticLabel[S]](override protected val timeLimitSecs: Double = 10.0) extends CkyParserWithList[SemanticParseNode[S]] {
  protected type Node = SemanticParseNode[S]

  var dict: String => Seq[(S, SemanticState)] = {_ => Seq()}

  def parse(str: String, tokenizer: String => IndexedSeq[String] = defaultTokenizer): SemanticParseResult[S] = {
    parse(tokenizer(str))
  }

  def parse(tokens: IndexedSeq[String]): SemanticParseResult[S] = {
    val chart = super.parseToChart(tokens)
    new SemanticParseResult(tokens, chart)
  }

  def loadSyntacticDict(syntacticDict: Map[String, Seq[S]]) {
    dict = { str: String =>
      for (entry <- syntacticDict.getOrElse(str, Seq())) yield {
        (entry, Ignored(str))
      }
    }
  }

  def flattenAndLoadSyntacticDict(syntacticDict: Map[Seq[String], Seq[S]]) {
    /**
     * Convert a map from Seq[A] -> Seq[B], to A -> Seq[B], flattening out the A's and concatenating any
     * duplicate entries of an A
     */
    def multikeymap[A, B](keymap: Map[Seq[A], Seq[B]]): Map[A, Seq[B]] = {
      val map = mutable.LinkedHashMap[A, Seq[B]]()
      for {
        (keys, values) <- keymap
        key <- keys
      } {
        if (map.contains(key)) {
          map(key) ++= values
        } else {
          map(key) = values
        }
      }
      map.toMap
    }

    loadSyntacticDict(multikeymap[String, S](syntacticDict))
  }

  def loadCcgBankLexicon(path: String) {
    // @todo: Incorporate the probabilities of each ccg category for each term

    val lexiconMap: mutable.Map[String, mutable.ListBuffer[S]] = mutable.Map()

    val file = Source.fromFile(path)
    for (line <- file.getLines()) {
      val parts = line.split(" +")
      val term = parts(0)
      val parsedCategory: CategoryParser.ParseResult[CcgCat] = CategoryParser(parts(1))
      val prob = parts(4).toDouble
      if (parsedCategory.successful) {
        val cat: S = (parsedCategory.get % prob).asInstanceOf[S]
        if (lexiconMap contains term) {
          lexiconMap(term).append(cat)
        } else {
          lexiconMap(term) = mutable.ListBuffer(cat)
        }
      }
    }

    loadSyntacticDict(lexiconMap.toMap.mapValues(_.toSeq))
  }

  private def defaultTokenizer(str: String): IndexedSeq[String] = {
    str.split(" ").map(_.toLowerCase)
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
    val dict = Map[String, Seq[CcgCat]](
      "the" -> Seq(NP/N),
      "quick" -> Seq(N|N),
      "brown" -> Seq(N|N),
      "fox" -> Seq(N),
      "and" -> Seq(conj),
      "silly" -> Seq(N|N),
      "cat" -> Seq(N),
      "jump" -> Seq((S\NP)/PP),
      "over" -> Seq(PP/NP),
      "a" -> Seq(NP/N),
      "lazy" -> Seq(N|N),
      "dog" -> Seq(N)
    )

    val parser = new SemanticParser[CcgCat]()
    // parser.loadSyntacticDict(dict)
    parser.loadCcgBankLexicon("CCGbank.00-24.lexicon")

    val result = parser.parse("the quick brown ox and the silly cat jump over the lazy dog")
    println(result.bestParse)
    result.debugPrint()
  }
}
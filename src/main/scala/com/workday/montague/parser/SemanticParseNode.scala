package com.workday.montague.parser

import java.io.ByteArrayInputStream

import com.workday.montague.ccg.SyntacticLabel
import com.workday.montague.cky.{ParseToken, Spans}
import org.apache.commons.lang.StringEscapeUtils._
import com.workday.montague.semantics.{Lambda, Form, SemanticState}

import scala.reflect.ClassTag

abstract class SemanticParseNode[S <: SyntacticLabel[S]](val syntactic: S, val semantic: SemanticState, val exs: Set[ClassCastException]) {
  def mapLogicalForm[LF : ClassTag, T](fn: LF => T): Option[T] = {
    logicalFormOption[LF].map(fn)
  }

  def flatMapLogicalForm[LF : ClassTag, T](fn: LF => Option[T]): Option[T] = {
    logicalFormOption[LF].flatMap(fn)
  }

  def isSemanticComplete = semantic.isInstanceOf[Form[_]]

  /**
   * Convert semantic state to Option
   *
   * @bug there seems to be a bug in the Scala compiler that is not showing errors when callers of this guy don't
   *     supply the necessary ClassTag implicit context bound
   *
   * @return Some(lf) if semantic state is completed (that is, it's a Form), otherwise None
   */
  def logicalFormOption[LF : ClassTag] = {
    import scala.reflect.classTag
    // As a work-around for the bug above, we throw our own exception if the implicit isn't supplied
    if (implicitly[ClassTag[LF]] == classTag[Nothing]) throw new IllegalArgumentException("ClassTag not supplied by caller")
    semantic match {
      case Form(lf: LF) => Some(lf)
      case _ => None
    }
  }

  /**
   * A helper function to stringify SemanticParseNode, which operates recursively on children
   * SemanticParseNode.
   * @param indent What indent to add at each new line?
   * @param currentIndent The current indent level
   * @param withSemantics Default = false. Only true at the top-level, because we don't want to print
   *                      many subtrees that include child SemanticStates that are contained within this SemanticState.
   * @return
   */
  def toStringHelp(indent: String = "", currentIndent: String = "", withSemantics: Boolean = false): String

  /**
   * @return Output a parse string in graphviz dot format
   */
  def toDotString: String = "digraph G {\n" + toDotStringHelp("n") + "}"
  def toDotStringHelp(pre: String): String

  def toSvg: String = {
    val cmd = List("dot", "-Tsvg")
    val inputString = toDotString
    // @bug: Possible security hole?
    val is = new ByteArrayInputStream(inputString.getBytes("UTF-8"))
    import scala.sys.process._
    val out = (cmd #< is).lines_!
    out.mkString("\n")
       .replaceAllLiterally("Lambda", "&#955;")  // Pretty rendering of Lambdas, useful when FunctionReaderMacro is used.
  }

  def spans: Spans
  def parseTokens: List[ParseToken]
  def terminals: List[SemanticParseNode[S]]

  /// parseTokenString is the literal string of ParseTokens, even if there are star tokens (*TRACE*, etc.) included.
  def parseTokenString: String

  def allNodes: List[SemanticParseNode[S]] = this :: children.flatMap(_.allNodes)

  def children: List[SemanticParseNode[S]]

  /**
   * Find a node satisfying the predicate p among all nodes rooted at me
   */
  def find(p: SemanticParseNode[S] => Boolean) = allNodes.find(p)

  /**
   * Find a node with completed semantic equal to the passed in logical form
   * @param lf the logical form to search for
   */
  def findLogicalForm[LF : ClassTag](lf: LF) = find(_.mapLogicalForm[LF, Boolean](_ == lf).getOrElse(false))
}

/**
 * Terminals always come from the dictionary.
 * @note NaturalQueryParser objects don't use Terminal. Instead, they use NQTerminal.
 * @param s The syntactic label.
 * @param parseToken
 * @todo It's pretty inefficient to store the ParseToken here, because it gets duplicated so much. Instead,
 *       the chart (or something) should store all the ParseTokens, and the Terminals should just store the Span within
 *       the chart.
 * @tparam S The syntactic scheme. In our case, CcgCat
 */
case class Terminal[S <: SyntacticLabel[S]](s: S, m: SemanticState, parseToken: ParseToken, spans: Spans, x: Set[ClassCastException]) extends SemanticParseNode(s, m, x) {
  override def toString: String = toStringHelp(withSemantics = true)
  def toStringHelp(indent: String = "", currentIndent: String = "", withSemantics: Boolean = false): String = {
    val str = "('" + s.toString + "', " + parseToken.toString() + ")"
    if (withSemantics) m.toString + ", '" + str
    else str
  }

  def toDotStringHelp(pre: String): String = {
    pre + " [shape=none,style=filled,fillcolor=lightblue,margin=0,pad=0,label=\"" + escapeJava(s.toString + "\n" + m.toString + "\n" + parseTokenString) + "\"];\n" +
      "{rank = max; \"tokens\"; " + pre + "}\n"
  }

  def parseTokens = List(parseToken)
  def terminals = List(this)
  def parseTokenString = parseToken.tokenString
  def children = Nil
}

/**
 * Non-terminals are always instantiated by the com.workday.montague.parser.
 * It's always binary, i.e. always has two children SemanticParseNode.
 * However, we lose the original ordering information.
 * @warning Even though this is a case class, we can't actually do equality comparisons. That's because the SemanticState
 *         can contain lambda functions. In fact, we should probably throw an assert on equality comparisons.
 * @todo It's inefficient to store both children nodes, both from a memory perspective and the fact that it prevents
 *       dynamic programming. Instead, we should store the *signatures* of the children.
 * @param s The (non-terminal) syntactic label.
 * @param m
 * @param operatorNode The SemanticParseNode that modifies the argument. e.g. if this is an adjective, the operatorNode
 *                     is the adjective, and the argumentNode is the noun.
 * @param argumentNode
 * @tparam S The syntactic scheme. In our case, CcgCat
 */
case class NonTerminal[S <: SyntacticLabel[S]](s: S, m: SemanticState, operatorNode: SemanticParseNode[S], argumentNode: SemanticParseNode[S],
                                               x: Set[ClassCastException]) extends SemanticParseNode(s, m, x) {
  override def toString: String = toStringHelp(indent="  ", withSemantics=true)
  def toStringHelp(indent: String = "", currentIndent: String = "", withSemantics: Boolean = false): String = {
    val nextIndent = currentIndent + indent
    val operatorString = operatorNode.toStringHelp(indent=indent, currentIndent=nextIndent, withSemantics=false)
    val argumentString = argumentNode.toStringHelp(indent=indent, currentIndent=nextIndent, withSemantics=false)
    val str = s"($spans, '" + s.toString + "', " + operatorString + ", \n" + indent + argumentString + ")"

    if (withSemantics) meaningString + s",\n" + str
    else str
  }
  def toDotStringHelp(pre: String): String = {
    val sem = meaningString.replaceAll("(\\S),(\\S)", "$1, $2")  // Prettify a little by adding spaces after commas.
    val nodeText = escapeJava(sem + "\n" + s.toString)
    pre + " [shape=none,style=filled,fillcolor=lightblue,margin=0,pad=0,label=\"" + nodeText + "\"];\n" +
      s"$pre -> ${pre}1;\n" +
      s"$pre -> ${pre}2;\n" +
      node1.toDotStringHelp(pre+"1") + node2.toDotStringHelp(pre+"2")
  }

  val meaningString: String = m match {
    case l: Lambda[_] =>
      val rawString = l.toString

      // If we're using pretty function representations produced by FunctionReaderMacro,
      // do some magic here to generally handle bound variables correctly.
      if (rawString.contains(" => ")) {
        val predicate = operatorNode match {
          case nonTerminalNode: NonTerminal[S] =>
            // If the operatorNode is a NonTerminal node, then we grab its predicate so we get all previous bindings.
            nonTerminalNode.meaningString.split(" => ").drop(1).mkString(" => ").dropRight(1) // pred is everything after first "=>"
          case _ =>
            // Otherwise, we don't care, let's just use this node's semantics.
            rawString
        }

        if (operatorNode.semantic.toString.contains("Lambda(")) {
          val boundVariable = operatorNode.semantic.toString.split("Lambda\\(")(1).split(':')(0) // var is between "Lambda(" and ":"
          val boundArgument = argumentNode.semantic.toString

          predicate.replaceAll(s"\\b$boundVariable\\b", boundArgument)
        } else {
          predicate
        }
      } else {
        rawString
      }
    case _ => m.toString
  }

  private[this] def node1 = {
    if (operatorNode.spans < argumentNode.spans) operatorNode
    else argumentNode
  }
  private[this] def node2 = {
    if (!(operatorNode.spans < argumentNode.spans)) operatorNode
    else argumentNode
  }
  def spans: Spans = node1.spans + node2.spans
  def parseTokens = node1.parseTokens ++ node2.parseTokens
  def terminals = node1.terminals ++ node2.terminals
  def parseTokenString = node1.parseTokenString + " " + node2.parseTokenString
  def children = node1 :: node2 :: Nil
}

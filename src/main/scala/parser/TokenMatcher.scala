package parser

import ccg._
import semantics._

trait TokenMatcher[S] extends (String => Seq[(S, SemanticState)])

// matches integers as CCG nouns, and applies given transformation from Int to logical form representation
case class IntegerMatcher[LF](intToState: Int => LF) extends TokenMatcher[CcgCat] {
  def apply(str: String) = {
    try {
      Seq(
        (N, Form[LF](intToState(Integer.parseInt(str))))
      )
    } catch {
      case nfe: NumberFormatException => Nil
    }
  }
}
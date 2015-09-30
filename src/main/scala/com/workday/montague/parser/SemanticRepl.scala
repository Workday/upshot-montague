package com.workday.montague.parser

import com.workday.montague.ccg.SyntacticLabel
import com.workday.montague.semantics.Form

import io.Source.stdin

/**
 * Given a SemanticParser, SemanticRepl starts an interactive REPL session, where the
 * "Eval" step consists of parsing the user's input as an Action and pattern-matching
 * the Action with `performAction` to perform some operation on an internal model.
 *
 * @tparam S syntactic scheme (usually CcgCat)
 * @tparam A action type
 * @tparam M model type
 */
abstract class SemanticRepl[S <: SyntacticLabel[S], A, M](parser: SemanticParser[S]) {
  def main(args: Array[String]): Unit = {
    var model: M = initialModel

    print(">> ")
    for (line <- stdin.getLines()) {
      val bestParseOpt = parser.parse(line).bestParse
      bestParseOpt.map(_.semantic) match {
        case Some(Form(action: A)) =>
          performAction(model, action) match {
            case (newModel: M, output: String) =>
              model = newModel
              println(output)
          }
        case _ => onParseError(line)
      }
      print(">> ")
    }
  }

  /**
   * Given the model and an action, returns the updated model and string output.
   */
  def performAction(model: M, action: A): (M, String)

  /**
   * Called whenever semantic parsing fails.
   */
  def onParseError(line: String): Unit

  val initialModel: M
}

package parser

import ccg.SyntacticLabel
import semantics.Form

import io.Source.stdin

abstract class SemanticRepl[S <: SyntacticLabel[S], A, M](parser: SemanticParser[S]) {
  def main(args: Array[String]): Unit = {
    var model: M = initialModel

    print(">> ")
    for (line <- stdin.getLines()) {
      val bestParseOpt = parser.parse(line).bestParse
      bestParseOpt.map(_.semantic) match {
        case Some(Form(action: A)) => model = performAction(model, action)
        case _ => onParseError(line)
      }
      print(">> ")
    }
  }

  def performAction(model: M, action: A):  M

  def onParseError(line: String): Unit

  val initialModel: M
}

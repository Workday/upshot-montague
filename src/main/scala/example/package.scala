import ccg._
import parser._
import semantics._

package object example {
  case object Paren extends TerminalCat { val category = "Paren" } // syntactic category for parenthetical expressions
}

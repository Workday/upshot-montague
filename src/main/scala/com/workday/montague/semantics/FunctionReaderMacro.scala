package com.workday.montague.semantics

import scala.language.experimental.macros
import scala.reflect.macros.Context

/**
 * Import com.workday.montague.semantics.FunctionReaderMacro.λ
 * instead of com.workday.montague.semantics.λ
 * to make Lambdas store readable function representations for display purposes.
 *
 * Uses lots of Scala black magic behind the hood, requires the scalamacros-paradise compiler extension.
 */
object FunctionReaderMacro {
  object λ {
    def apply[LF](func: LF => _): SemanticState = macro applyMacroTotal[LF]

    def apply[LF](func: PartialFunction[LF, _]): SemanticState = macro applyMacroPartial[LF]

    def applyMacroTotal[LF: c.WeakTypeTag](c: Context)(func: c.Expr[LF => _]): c.Expr[LF] = {
      import c.universe._
      c.Expr(q"Lambda[${c.weakTypeOf[LF]}](new FunctionWrapper(SemanticImplicits.FuncToSemanticState($func), ${show(func.tree)})): SemanticState")
    }

    def applyMacroPartial[LF: c.WeakTypeTag](c: Context)(func: c.Expr[PartialFunction[LF, _]]): c.Expr[LF] = {
      import c.universe._
      c.Expr(q"Lambda[${c.weakTypeOf[LF]}](new FunctionWrapper(SemanticImplicits.PartialFuncToSemanticState($func), ${show(func.tree)})): SemanticState")
    }
  }
}

trait Wrapper {
  val representation: String
  override def toString: String = Wrapper.cleanUp(representation)
}

// A unary total function with a specified string representation.
class FunctionWrapper[P, R](val fn: P => R, val representation: String) extends (P => R) with Wrapper {
  def apply(p: P): R = fn(p)
}

// A unary partial function with a specified string representation.
class PartialFunctionWrapper[P, R](val fn: PartialFunction[P, R], val representation: String) extends PartialFunction[P, R] with Wrapper {
  def apply(p: P): R = fn(p)
  def isDefinedAt(x: P): Boolean = fn.isDefinedAt(x)
}

object Wrapper {
  /** Clean up function representation by removing a bunch of ugly garbage.
   *  For example:
   *    From: Lambda(((o: myPackage.ObjectType) => (com.workday.montague.semantics.Lambda.apply[myPackage.Condition](new com.workday.montague.semantics.FunctionWrapper[com.workday.montague.semantics.SemanticState,com.workday.montague.semantics.SemanticState](com.workday.montague.semantics.SemanticImplicits.FuncToSemanticState[myPackage.Condition](((c: myPackage.Condition) => Choose.apply(o, c))), "((c: myPackage.Condition) => Choose.apply(o, c))")): com.workday.montague.semantics.SemanticState)))
   *      to: Lambda(o: ObjectType => Lambda(c: Condition => Choose(o, c)))
   */
  def cleanUp(representation: String): String = {
    //println(representation)
    representation
      .replaceAll("\n", " ")
      .replaceAll("  +", " ")
      .replaceAllLiterally("com.workday.montague.semantics.", "")
      .replaceAll(""", ".*?[^\\]"\)""", ")")  // Remove expressions in quotes.
      .replaceAll("""\(reflect[\w.]*classType\[[\w.$]*\]\(classOf\[[\w.$]*\]\)\)""", "")  // Remove (reflect.this.ManifestFactory.classType[...](classOf[...]))
      .replaceAll("""(?<!isInstanceOf|: Seq)\[.*?\]\(""", "(")  // Omit type parameters (except in the context of isInstanceOf[...) or argument types)
      .replaceAll("""\.([\+\-*/]|:\+|\+\+)\(""", " $1 (")  // better display of infix operators, e.g. 1.+(2) => 1 + (2)
      .replaceAllLiterally(").->(", ", ")  // better display of tuples in the spirit of the above, e.g. (1).->(2) => (1, 2)
      .replaceFirstRepeatedly("""new FunctionWrapper\(SemanticImplicits\.FuncToSemanticState\((.*?)\)\): SemanticState""", "$1")  // Remove invocations of FunctionWrapper itself
      .replaceAll("""\((\S*)\)\((\w+\.)*(\w+)\.canBuildFrom\[[\w.]*\]\)""", "$3($1)")  // e.g. (x)(collection.this.Seq.canBuildFrom[T]) => Seq(x)
      .replaceAll("""\((\S*)\.apply\((\S+(?:\,\s?\S+)*)\)\)\((\w+\.)*(\w+)\.canBuildFrom\[[\w.]*\]\)""", "$4($1($2))")  // e.g. (x.apply(y))(collection.this.Seq.canBuildFrom[T]) => Seq(x(y))
      .replaceAllLiterally("collection.this.", "")
      .replaceAllLiterally(".apply", "")
      .replaceAllLiterally("Seq", "List")  // (just for consistency)
      .replaceAll("""\b(\w+\.)*([A-Z]\w+)\b""", "$2")  // e.g. package.Class => Class
      .replaceAllLiterally("Predef.", "")
      .replaceAll(""", \w+\$default\$\d+""", "")  // Ignore default parameters (e.g. SomeObject$default$3).
      .replaceAll("""\(\w+\$default\$\d\)""", "()")  // Ignore default parameters (e.g. SomeObject(SomeObject$default$1) => SomeObject()).
      .replaceAll("""[a-zA-Z]+2[a-zA-Z]+\(""", "(")  // remove all implicit conversions
      .replaceAll("""\(\w*\.canBuildFrom(\[[\w.]*\]|\(conforms\[.*\]\))\)""", "")  // yet another canBuildFrom situation, e.g. (List.canBuildFrom(conforms[(String, String)])) -> ''
      .replaceFirstRepeatedly("""\(Lambda\((.*)\)\)""", "Lambda($1)")  // Remove unnecessary parentheses around Lambdas
      .replaceFirstRepeatedly("""\(\(\((\w*: \w*)\) => (.*)\)\)""", "($1 => $2)")  // Remove unnecessary parentheses around Lambda parameters
      .replaceFirstRepeatedly("""\(\((\w*: \w*)\) => (.*)\)""", "($1 => $2)")  // ibid
      .replaceFirst("""^\((.*)\)""", "$1")  // and remove outermost set of extra parens (only once)
      .normalizeTrailingParentheses
  }

  implicit class RichString(str: String) {
    def replaceFirstRepeatedly(regex: String, replacement: String): String = {
      val replaced = str.replaceFirst(regex, replacement)
      if (replaced == str) {
        replaced
      } else {
        replaced.replaceFirstRepeatedly(regex, replacement)
      }
    }

    def normalizeTrailingParentheses: String = {
      val numExtraRightParens = str.count(_ == ')') - str.count(_ == '(')
      if (numExtraRightParens > 0) {
        str.dropRight(numExtraRightParens)
      } else if (numExtraRightParens < 0) {
        s"$str${")" * -numExtraRightParens}"
      } else {
        str
      }
    }
  }
}

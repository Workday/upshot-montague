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
    representation
      .replaceAll("\n", "")
      .replaceAllLiterally("com.workday.montague.semantics.", "")
      .replaceAll(""", ".*?[^\\]"\)""", ")")  // Remove expressions in quotes.
      .replaceAll("""\(reflect[\w.]*classType\[[\w.$]*\]\(classOf\[[\w.$]*\]\)\)""", "")  // Remove (reflect.this.ManifestFactory.classType[...](classOf[...]))
      .replaceAll("""\[.*?\]\(""", "(")  // Omit type parameters.
      .replaceAll("""\.([\+\-*/]|:\+|\+\+)\(""", " $1 (")  // e.g. 1.+(2) => 1 + (2)
      .replaceAll("""SemanticImplicits\.FuncToSemanticState\((.*?)\)""", "$1")
      .replaceAll("""\((\S*)\)\((\w+\.)*(\w+)\.canBuildFrom\[[\w.]*\]\)""", "$3($1)")  // e.g. (x)(collection.this.Seq.canBuildFrom[T]) => Seq(x)
      .replaceAll("""\((\S*)\.apply\((\S+(?:\,\s?\S+)*)\)\)\((\w+\.)*(\w+)\.canBuildFrom\[[\w.]*\]\)""", "$4($1($2))")  // e.g. (x.apply(y))(collection.this.Seq.canBuildFrom[T]) => Seq(x(y))
      .replaceAllLiterally("collection.this.", "")
      .replaceAllLiterally(".apply", "")
      .replaceAllLiterally(": SemanticState", "")
      .replaceAllLiterally("Seq", "List")  // (just for consistency)
      .replaceAllRepeatedly("""new FunctionWrapper\((.*?)\)""", "$1")  // remove all invocations of FunctionWrapper() itself
      .replaceAll(""": [a-z]*\.""", ": ")  // Trim package names.
      .replaceAll("""^\((.*)\)$""", "$1")  // Remove parentheses ...
      .replaceAll("""\((.*)\) =>""", "$1 =>")  // etc ...
      .replaceAll("""=> \((.*)\)""", "=> $1")  // etc ...
      .replaceAll("""=> \((.*)\)""", "=> $1")  // etc ...
      .replaceAllRepeatedly("""\(\((.* => .*)\)\)""", "($1)")  // etc ...
      .replaceAll("""\b\w+\.([A-Z]\w+)\b""", "$1")  // e.g. package.Class => Class
      .replaceAll(""", \w+\$default\$\d+""", "")  // Ignore default parameters (e.g. SomeObject$default$3).
  }

  implicit class RichString(str: String) {
    def replaceAllRepeatedly(regex: String, replacement: String): String = {
      val replaced = str.replaceAll(regex, replacement)
      if (replaced == str) {
        replaced
      } else {
        replaced.replaceAllRepeatedly(regex, replacement)
      }
    }
  }
}

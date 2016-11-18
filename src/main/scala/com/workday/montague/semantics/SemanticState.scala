package com.workday.montague.semantics

import SemanticImplicits._
import com.workday.montague.util.StringUtil

/**
 * This is either a lambda, or a completed state.
 * It can be in the Lambda state, where it is waiting for arguments.
 * Or it can be in a Form state, where it has all the arguments it wants, and is fully resolved as a predicate.
 * Example:
 *  "biotechnology" starts as Lambda state lambda x, waiting for an argument.
 *  When it takes an argument, it returns filter(x, BasePredicate(industry = biotech)), which is a Form.
 *
 * SemanticStates consume *one* argument at a time (they are curried).
 */
trait SemanticState {
  def apply(arg: SemanticState): SemanticState = {
    this match {
      case Lambda(k) => arg match {
        case Ignored(tree) => k(Form(tree))  // this Ignored -> Form transformation allows us to combine semantic
                                             // entries with purely syntactic (e.g. CCGbank entries)
        case _ => try {
          k(arg)
        } catch {
          case e: ClassCastException => Nonsense
        }
      }
      case Form(value) => Nonsense
      case Nonsense => Nonsense
      case Ignored(tree) => arg match {
        case Ignored(tree2) => Ignored(s"$tree($tree2)")
        case _ => Nonsense
      }
    }
  }
}

/// Lambda means the semantic state is in progress, and is still consuming arguments.
case class Lambda[LF](k: SemanticState => SemanticState) extends SemanticState

/// Form (β-normal form) means the semantic state is not consuming arguments.
case class Form[LF](value: LF) extends SemanticState {
  override def toString: String = value.toString
}

/// Represents a parse with no valid semantic outputs
case object Nonsense extends SemanticState

/// Represents a parse with more than one valid semantic output
case class Ambiguous(options: Set[SemanticState]) extends SemanticState {
  override def toString: String = s"Ambiguous(${options.mkString(", ")})"
}

/// Ignores semantics and simply stores the dependency tree (for purely syntactic parses)
case class Ignored(tree: String) extends SemanticState {
  override def toString: String = StringUtil.toPrettyTree(tree)
}

object λ {
  def apply[LF](func: LF => _): SemanticState = {
    Lambda(func)
  }

  def apply[LF](func: PartialFunction[LF, _]): SemanticState = {
    Lambda(func)
  }
}

object SemanticImplicits {
  implicit def LFToSemanticState[LF](value: LF): SemanticState = Form(value)

  implicit def FuncToSemanticState[LF](func: LF => _): (SemanticState => SemanticState) = {
    case Form(value) =>
      value match {
        case lf: LF =>
          val result = func(lf)
          result match {
            case fn: SemanticState => fn
            case res: LF => Form(res)
          }
        case _ => Nonsense
      }
    case _ => Nonsense
  }

  implicit def PartialFuncToSemanticState[LF](func: PartialFunction[LF, _]): (SemanticState => SemanticState) = {
    case Form(value) =>
      value match {
        case lf: LF =>
          if (func.isDefinedAt(lf)) {
            val result = func(lf)
            result match {
              case fn: SemanticState => fn
              case res: LF => Form(res)
            }
          } else {
            Nonsense
          }
        case _ => Nonsense
      }
    case _ => Nonsense
  }
}

package com.workday.montague.semantics

import SemanticImplicits._
import com.workday.montague.util.StringUtil

import scala.util.matching.Regex

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
          case ex: ClassCastException => Nonsense(Set(ex))
        }
      }
      case Form(_) => Nonsense()
      case Nonsense(exs) => arg match {
        case Nonsense(exs2) => Nonsense(exs ++ exs2)
        case _ => Nonsense(exs)
      }
      case Ignored(tree) => arg match {
        case Ignored(tree2) => Ignored(s"$tree($tree2)")
        case _ => Nonsense()
      }
    }
  }

  /**
    * Returns strings representing argument types for all curried parameters,
    * in order of application, inside a Lambda.
    *
    * Returns Seq.empty for anything that's not a Lambda, or if
    * [[FunctionReaderMacro]] wasn't used to create the Lambda.
    */
  def argTypeStrings: Seq[String] = {
    SemanticState.lambdaArgTypeRegex.findAllMatchIn(toString).toList.map(_.group(1))
  }
}

object SemanticState {
  /** Matches type parameters in the string representation of a Lambda, as produced by [[FunctionReaderMacro]]. */
  lazy val lambdaArgTypeRegex: Regex = """Lambda\(\w*: (\w*) =>""".r
}

// Lambda means the semantic state is in progress, and is still consuming arguments.
case class Lambda[LF](k: SemanticState => SemanticState) extends SemanticState {
}

// Form (β-normal form) means the semantic state is not consuming arguments.
case class Form[LF](value: LF) extends SemanticState {
  override def toString: String = value.toString
}

// Represents a parse with no valid semantic outputs
case class Nonsense(ex: Set[ClassCastException] = Set()) extends SemanticState

// Represents a parse with more than one valid semantic output
case class Ambiguous(options: Set[SemanticState]) extends SemanticState {
  override def toString: String = s"Ambiguous(${options.mkString(", ")})"
}

// Ignores semantics and simply stores the dependency tree (for purely syntactic parses)
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

  implicit def FuncToSemanticState[LF : Manifest](func: LF => _): SemanticState => SemanticState = {
    case Form(value) =>
      value match {
        case lf: LF =>
          val result = func(lf)
          result match {
            case fn: SemanticState => fn
            case res: LF => Form(res)
          }
        case _ => Nonsense()
      }
    case _ => Nonsense()
  }

  implicit def PartialFuncToSemanticState[LF : Manifest](func: PartialFunction[LF, _]): SemanticState => SemanticState = {
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
        case _ => Nonsense()
      }
    case _ => Nonsense()
  }
}

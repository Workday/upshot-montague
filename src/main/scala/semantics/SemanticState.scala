package semantics

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
      case Func(k) => k(arg)
      case Form(value) => Nonsense
      case Nonsense => Nonsense
      case Ignored(tree) => arg match { case Ignored(tree2) => Ignored(s"$tree($tree2)"); case _ => Nonsense }
    }
  }
}

/// Func means the semantic state is in progress, and is still consuming arguments.
/// Func is the internal representation of lambda functions -- use Lambda instead to define
/// lambda functions
case class Func[LF](k: SemanticState => SemanticState) extends SemanticState

/// Form (β-normal form) means the semantic state is not consuming arguments.
case class Form[LF](value: LF) extends SemanticState

case object Nonsense extends SemanticState

/// Ignores semantics and stores the dependency tree (for purely syntactic parses)
case class Ignored(tree: String) extends SemanticState

object Lambda {
  def apply[LF](func: LF => _): SemanticState = Func {
    case Form(value) => {
      value match {
        case lf: LF =>
          val result = func(lf)
          result match {
            case fn: SemanticState => fn
            case res: LF => Form(res)
          }
        case _ => Nonsense
      }
    }
    case _ => Nonsense
  }
}

object λ {
  def apply[LF](func: LF => _): SemanticState = {
    Lambda(func)
  }
}
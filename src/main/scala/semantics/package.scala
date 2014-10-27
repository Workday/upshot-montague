import scala.reflect.ClassTag

package object semantics {
  def orderedApply[LF](num: Int, fn: List[SemanticState] => SemanticState): SemanticState = {
    def step(currentArgs: List[SemanticState], nRemaining: Int): SemanticState => SemanticState = {
      newArg: SemanticState =>
        val newArgs = currentArgs :+ newArg
        if (nRemaining > 1) {
          Lambda(step(newArgs, nRemaining - 1))
        } else {
          val newNode = fn(newArgs)
          newNode
        }
    }
    Lambda(step(List(), num))
  }

  // assumes children have all been parsed
  // may not be the case
  // but we can have parameterized ones that
  // allow the children to not be parsed
  def allDoneFunction[LF : ClassTag](fn: List[LF] => LF): List[SemanticState] => SemanticState = {
    list: List[SemanticState] =>
      val formsList = list.flatMap {
        case Form(value: LF) => Some(value)
        case _ => None
      }

      if (formsList.size < list.size) {
        Nonsense
      } else {
        Form(fn(formsList))
      }
  }

  def allDoneFunctionWithMatch[LF : ClassTag](fn: PartialFunction[List[LF], LF]): List[SemanticState] => SemanticState = {
    list: List[SemanticState] =>
      val formsList = list.flatMap {
        case Form(value: LF) => Some(value)
        case _ => None
      }

      if (formsList.size < list.size) {
        Nonsense
      } else {
        if (fn.isDefinedAt(formsList)) {
          try {
            Form(fn(formsList))
          } catch {
            case _: MatchError => Nonsense
          }
        } else {
          Nonsense
        }
      }
  }

  def argMatch[LF : ClassTag](n: Int, fn: PartialFunction[List[LF], LF]) = orderedApply(n, allDoneFunctionWithMatch(fn))

  /**
   * Lift a partial function fn: LF => x to a Lambda SemanticState that consumes semantic states
   * in the Form(lf) state, applies the fn to the lf, then returns the result, also lifted.
   *
   * Tye type x can be another partial function, so that you can lift curried partial functions
   * e.g. LF => LF => LF, or x can be LF.
   *
   * This works for the vast majority of lambda's that consume complete semantic predicates i.e.
   * their arguments are not other lambdas.
   *
   * Example usage:
   *
   *     lift[LFNode]({ case rel: RelationLike => Limit(rel, 5) })
   *
   * The argument has type `PartialFunction[LFNode, PartialFunction[LFNode, LFNode]]`.
   *
   * "Lifting" is a functional programming term that roughly means "wrapping".
   *
   * @todo Migrate all existing semantics that use argMatch to this, since this is much simpler
   *       and a more direct implementaiton of lambdas.  This can replace argMatch as well as
   *       orderedApply etc.  This also doesn't require explicit declaration of the number of
   *       arguments, although it might be desirable to create variants lift1, lift2, etc.
   */
  def lift[LF : ClassTag](fn: PartialFunction[LF, _]): SemanticState = {
    def step(fn: PartialFunction[LF, _]): SemanticState => SemanticState = {
      newArg: SemanticState =>
        newArg match {
          case Form(value: LF) =>
            if (fn.isDefinedAt(value)) {
              val result = fn(value)
              result match {
                case nextfn: PartialFunction[LF, _] => Lambda(step(nextfn))
                case lf: LF => Form(lf)
              }
            } else {
              Nonsense
            }
          case _ => Nonsense
        }
    }
    Lambda(step(fn))
  }

  /**
   * These overloads are necessary because Scala's partial function syntax doesn't allow syntax like:
   *
   *      lift(case x => case y => z)
   *
   * Because the argument type of the anonymous function { case y => z } is unspecified.  So you'll
   * get a compiler error.
   *
   * This overload lets it work because the argument type of the inner partial function can be
   * derived from the method signature here (similarly for the outer partial function).
   */
  def lift2[LF : ClassTag](fn: PartialFunction[LF, PartialFunction[LF, LF]]) = lift(fn)

  /**
   * A conjunction is a kind of 3-argument lambda expression. The first two arguments are functions that
   * are both applied to the third argument. The two results are then combined together by the specified
   * (LF, LF) => SemanticState joiner function.
   *
   * For example, a simple AND between two Filter nodes operating on the same relation can be defined as follows:
   *    conjunction[LFNode] {
   *      case (Filter(r1: RelationLike, p: Predicate), Filter(r2: RelationLike, q: Predicate)) if r1 equals r2 =>
   *        Done[LFNode](Filter(Filter(r1, p), q))
   *    }
   */
  def conjunction[LF](joiner: PartialFunction[(LF, LF), SemanticState]): SemanticState = {
    Lambda[LF] { g: SemanticState =>
      Lambda[LF] { f: SemanticState =>
        Lambda[LF] { x: SemanticState =>
          val fx: SemanticState = f.apply(x)
          val gx: SemanticState = g.apply(x)
          (fx, gx) match {
            case (Form(left: LF), Form(right: LF)) => joiner.applyOrElse[(LF, LF), SemanticState]((left, right), _ => Nonsense)
            case _ => Nonsense
          }
        }
      }
    }
  }

  // the ordering of operands doesn't really belong here
  def of: SemanticState = orderedApply(2, {
    case (rhs: SemanticState) :: (lhs: SemanticState) :: Nil => lhs.apply(rhs)
    case _ => Nonsense
  })

  def identity: SemanticState = new SemanticState {
    override def apply(arg: SemanticState): SemanticState = arg
  }
}
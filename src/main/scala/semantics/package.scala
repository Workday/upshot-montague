import scala.reflect.ClassTag

package object semantics {
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

  def identity: SemanticState = new SemanticState {
    override def apply(arg: SemanticState): SemanticState = arg
  }
}
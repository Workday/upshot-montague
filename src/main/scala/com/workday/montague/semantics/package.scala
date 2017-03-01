package com.workday.montague

package object semantics {
  def identity: SemanticState = new SemanticState {
    override def apply(arg: SemanticState): SemanticState = arg
    override def toString: String = "identity"
  }

  /**
   * A conjunction is a kind of 3-argument lambda expression. The first two arguments are functions that
   * are both applied to the third argument. The two results are then combined together by the specified
   * (LF, LF) => SemanticState joiner function.
   *
   * For example, a simple AND between two Filter nodes operating on the same relation can be defined as follows:
   *    conjunction[LFNode] {
   *      case (Filter(r1: RelationLike, p: Predicate), Filter(r2: RelationLike, q: Predicate)) if r1 equals r2 =>
   *        Done[LFNode](Filter(Filter(r1, p), q))+7
   *    }
   */
  def conjunction[LF](joiner: PartialFunction[(LF, LF), SemanticState]): SemanticState = {
    Lambda[LF] { g: SemanticState =>
      Lambda[LF] { f: SemanticState =>
        Lambda[LF] { x: SemanticState =>
          val fx: SemanticState = f.apply(x)
          val gx: SemanticState = g.apply(x)
          (fx, gx) match {
            case (Form(left: LF), Form(right: LF)) => joiner.applyOrElse[(LF, LF), SemanticState]((left, right), _ => Nonsense())
            case _ => Nonsense()
          }
        }
      }
    }
  }
}

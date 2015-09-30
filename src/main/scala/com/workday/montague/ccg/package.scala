package com.workday.montague

package object ccg {
  case object N extends TerminalCat { val category = "N" }
  case object NP extends TerminalCat { val category = "NP" }
  case object PP extends TerminalCat { val category = "PP" }
  case object S extends TerminalCat { val category = "S" }

  val conj = Conj // sometimes lowercase convention is used for conj

  // Helper object to enable shorthand for identity categories:
  //    X|X = IdentityCat
  //    X/X = ForwardIdentityCat
  //    X\X = BackwardIdentityCat
  case object X extends TerminalCat {
    val category = ""

    override def |(arg: CcgCat): CcgCat = arg match {
      case X => IdentityCat
      case _ => X
    }

    override def /(arg: CcgCat): CcgCat = arg match {
      case X => ForwardIdentityCat
      case _ => X
    }

    override def \(arg: CcgCat): CcgCat = arg match {
      case X => BackwardIdentityCat
      case _ => X
    }
  }
}

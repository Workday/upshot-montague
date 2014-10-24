package ccg

sealed trait CcgCat extends SyntacticLabel[CcgCat] {
  val category: String
  val label: Option[String] = None
  val prob: Double = 1.0

  def matches(filter: CcgCat): Boolean = filter.label match {
    case Some(_) => category == filter.category && label == filter.label
    case None => category == filter.category
  }

  def getForwardApplication(right: CcgCat): Option[CcgCat]
  def getBackwardApplication(left: CcgCat): Option[CcgCat]

  def deriveRightward(arg: CcgCat): List[CcgCat] = getForwardApplication(arg).map(ProbabilisticCat(_, prob * arg.prob)).toList
  def deriveLeftward(arg: CcgCat): List[CcgCat] = getBackwardApplication(arg).map(ProbabilisticCat(_, prob * arg.prob)).toList

  def \(arg: CcgCat): CcgCat = BackwardsCat(this, arg)
  def /(arg: CcgCat): CcgCat = ForwardCat(this, arg)
  def |(arg: CcgCat): CcgCat = ForwardsBackwardsCat(this, arg)
  def apply(label: String): CcgCat = LabelledCat(this, Some(label))
  def %(pr: Double) = ProbabilisticCat(this, pr)

  override def toString = category + (if (label.isDefined) s"[${label.get}]" else "") + (if (prob != 1.0) s"%$prob" else "")
  override val score = prob
}

case class ForwardCat(result: CcgCat, arg: CcgCat) extends CcgCat {
  def getForwardApplication(right: CcgCat): Option[CcgCat] = if (right matches arg) Some(result) else None
  def getBackwardApplication(arg: CcgCat): Option[CcgCat] = None
  val category = s"(${result}/${arg})"
}
case class BackwardsCat(result: CcgCat, arg: CcgCat, name: Option[String] = None) extends CcgCat {
  def getForwardApplication(right: CcgCat): Option[CcgCat] = None
  def getBackwardApplication(left: CcgCat): Option[CcgCat] = if (left matches arg) Some(result) else None
  val category = s"(${result}\\${arg})"
}
// why represent it like this and not as two categories in the lexicon - see "Online Learning of Relaxed CCG Grammars" which takes a similar approach
case class ForwardsBackwardsCat(result: CcgCat, arg: CcgCat, name: Option[String] = None) extends CcgCat {
  def getForwardApplication(right: CcgCat): Option[CcgCat] = if (right matches arg) Some(result) else None
  def getBackwardApplication(left: CcgCat): Option[CcgCat] = if (left matches arg) Some(result) else None
  val category = s"(${result}|${arg})"
}

case class LabelledCat(cat: CcgCat, override val label: Option[String]) extends CcgCat {
  def getForwardApplication(right: CcgCat): Option[CcgCat] = None
  def getBackwardApplication(left: CcgCat): Option[CcgCat] = None
  val category = cat.category
}

case class ProbabilisticCat(cat: CcgCat, override val prob: Double) extends CcgCat {
  def getForwardApplication(right: CcgCat): Option[CcgCat] = cat.getForwardApplication(right)
  def getBackwardApplication(left: CcgCat): Option[CcgCat] = cat.getBackwardApplication(left)
  val category = cat.category
}

trait TerminalCat extends CcgCat {
  def getForwardApplication(right: CcgCat): Option[CcgCat] = None
  def getBackwardApplication(left: CcgCat): Option[CcgCat] = None
}

case object N extends TerminalCat { val category = "N" }
case object NP extends TerminalCat { val category = "NP" }
case object PP extends TerminalCat { val category = "PP" }
case object S extends TerminalCat { val category = "S" }

case object IdentityCat extends CcgCat {
  def getForwardApplication(right: CcgCat): Option[CcgCat] = Some(right)
  def getBackwardApplication(left: CcgCat): Option[CcgCat] = Some(left)
  val category = "X|X"
}

case object ForwardIdentityCat extends CcgCat {
  def getForwardApplication(right: CcgCat): Option[CcgCat] = Some(right)
  def getBackwardApplication(left: CcgCat): Option[CcgCat] = None
  val category = "X/X"
}

case object BackwardIdentityCat extends CcgCat {
  def getForwardApplication(right: CcgCat): Option[CcgCat] = None
  def getBackwardApplication(left: CcgCat): Option[CcgCat] = Some(left)
  val category = "X\\X"
}

// Conjunction: (X\X)/X for any category X
case object Conj extends CcgCat {
  def getForwardApplication(right: CcgCat): Option[CcgCat] = Some(right\right)
  def getBackwardApplication(right: CcgCat): Option[CcgCat] = None
  val category = "(X\\X)/X"
}
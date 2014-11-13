package ccg

/**
 * A set of syntactic labels along with rules that indicate how labels can be combined to derive
 * new labels.  Implementations create their own class hierarchies that extend SyntacticLabel[MyImpl]
 */
trait SyntacticLabel[S <: SyntacticLabel[S]] {
  /** Derivation rules for this label being joined with a label to its left or right */
  def deriveRightward(arg: S): List[S]
  def deriveLeftward(arg: S): List[S]

  // optionally, syntactic labels can have scores attached to them (e.g. for probabilistic parsing)
  val score: Double = 1.0
}

import scala.reflect.ClassTag

/**
 * Builds our proprietary sematic parser, using the package machinery
 * in Parser.
 * It has:
 *  - our core dictionary
 *  - our parse tree definition
 *  - our translation to logical form, and from logical form to
 *    logical SQL, and from logical SQL to physical SQL (a string).
 */
package object natural {
  type NQDict = parser.CcgDict
  type MutableNQDict = parser.MutableCcgDict
  type NQParseNode = parser.SemanticParseNode[CcgCat]

  type SemanticState = parser.semantics.SemanticState
  type Done[LF] = parser.semantics.Done[LF]
  val Done = parser.semantics.Done
  def argMatch[LF : ClassTag](n: Int, fn: PartialFunction[List[LF], LF]) = parser.semantics.argMatch[LF](n, fn)
  def lift[LF : ClassTag](fn: PartialFunction[LF, _]) = parser.semantics.lift[LF](fn)

  type CcgCat = parser.ccg.CcgCat
  val ForwardCat = parser.ccg.ForwardCat
  val BackwardsCat = parser.ccg.BackwardsCat
  val Noun = parser.ccg.Noun
  val Preposition = parser.ccg.Preposition
  val CompoundingNoun = parser.ccg.CompoundingNoun
  val Adjective = parser.ccg.Adjective
  val OperatingPreposition = parser.ccg.OperatingPreposition
  val TransitiveVerb = parser.ccg.TransitiveVerb
  val Sentence = parser.ccg.Sentence
  val ForwardIdentityCat = parser.ccg.ForwardIdentityCat
  val BackwardIdentityCat = parser.ccg.BackwardIdentityCat
}

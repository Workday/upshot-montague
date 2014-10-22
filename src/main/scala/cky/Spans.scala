package cky

// We call this Spans to avoid a name conflict with IndexedSeqOptimized.span
case class Spans(start: Int, end: Int) {
  assert(start < end)

  override def toString: String = s"[$start,$end]"

  def length = end - start

  def +(s2: Spans): Spans = {
    val s1 = this
    assert(s1.end == s2.start)
    Spans(s1.start, s2.end)
  }

  def <(s2: Spans): Boolean = {
    val s1 = this
    assert(s1.end <= s2.start || s2.end <= s1.start, "The spans overlap")
    s1.end <= s2.start
  }

  def isContainedIn(s2: Spans): Boolean = {
    this.start >= s2.start && this.end <= s2.end
  }
}
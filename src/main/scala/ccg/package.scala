
package object ccg {
  val N = Noun
  val S = Sentence

  // Simplified - no noun phrases
  val CompoundingNoun = N\N
  val Adjective = N|N
  val Preposition = N/(N\N)
  val OperatingPreposition = (N\(N\N))/N // takes a noun forward, then takes a compounding non backward
  // this is for "sum of revenue" which we basically transform to "revenue sum" or "average of revenue" which turns into "revenue average"
  // we have "average" and "sum" pre-defined as "compounding noun" category
  // so i made "of" transform the compounding nouns into the form "X of Y" where X is the compounding noun and we transform it to "Y X" like "revenue average"
  val TransitiveVerb = (S\N)/N
}
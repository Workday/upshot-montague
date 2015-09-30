package com.workday.montague.cky

/**
 * A ParseToken input to the com.workday.montague.parser.
 * @note The terminology "token" is overloaded. We use it here to mean a phrasal token input to the com.workday.montague.parser,
 *       which is a sequence of tokens.
 *       A "Token" is single *token* that is the output from tokenizing the input query,
 * @param tokens A sequence of tokens that the ParseToken comprises.
 */
case class ParseToken(tokens: Seq[String]) {
  def tokenString: String = tokens.mkString(" ")
  override def toString: String = s"'$tokenString'"
}

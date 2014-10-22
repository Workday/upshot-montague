package ccg

import scala.util.parsing.combinator.RegexParsers

object CategoryParser extends RegexParsers {
  def noun: Parser[CcgCat] = "N" ^^ {_ => Noun}
  def sentence: Parser[CcgCat] = "S" ^^ {_ => Sentence}

  def backward: Parser[CcgCat] = (term ~ "\\" ~ term) ^^ { case t1 ~ _ ~ t2 => t1\t2 }
  def forward: Parser[CcgCat] = (term ~ "/" ~ term) ^^ { case t1 ~ _ ~ t2 => t1/t2 }
  def forwardBackward: Parser[CcgCat] = (term ~ "|" ~ term) ^^ { case t1 ~ _ ~ t2 => t1|t2 }

  def labelledAtom: Parser[CcgCat] = (atom ~ "[" ~ "[a-z]*".r ~ "]") ^^ { case atom ~ _ ~ label ~ _ => atom(label) }

  def atom: Parser[CcgCat] = noun | sentence
  def term = paren | labelledAtom | atom
  def op = backward | forward | forwardBackward
  def expr = op | term

  def paren: Parser[CcgCat] = ("(" ~ expr ~ ")") ^^ { case _ ~ contents ~ _ => contents }

  def apply(s: String) = parseAll(expr, s)
}
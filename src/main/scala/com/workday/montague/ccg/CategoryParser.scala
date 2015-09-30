package com.workday.montague.ccg

import scala.util.parsing.combinator.RegexParsers

object CategoryParser extends RegexParsers {
  def n: Parser[CcgCat] = "N" ^^ {_ => N}
  def np: Parser[CcgCat] = "NP" ^^ {_ => NP}
  def pp: Parser[CcgCat] = "PP" ^^ {_ => PP}
  def s: Parser[CcgCat] = "S" ^^ {_ => S}
  def conj: Parser[CcgCat] = "[cC]onj".r ^^ {_ => Conj}

  def backward: Parser[CcgCat] = (term ~ "\\" ~ term) ^^ { case t1 ~ _ ~ t2 => t1\t2 }
  def forward: Parser[CcgCat] = (term ~ "/" ~ term) ^^ { case t1 ~ _ ~ t2 => t1/t2 }
  def forwardBackward: Parser[CcgCat] = (term ~ "|" ~ term) ^^ { case t1 ~ _ ~ t2 => t1|t2 }

  def labelledAtom: Parser[CcgCat] = (atom ~ "[" ~ "[a-z]*".r ~ "]") ^^ { case atom ~ _ ~ label ~ _ => atom(label) }

  def atom: Parser[CcgCat] = np | n | pp | s | conj
  def term = paren | labelledAtom | atom
  def op = backward | forward | forwardBackward
  def expr = op | term

  def paren: Parser[CcgCat] = ("(" ~ expr ~ ")") ^^ { case _ ~ contents ~ _ => contents }

  def apply(s: String) = parseAll(expr, s)
}

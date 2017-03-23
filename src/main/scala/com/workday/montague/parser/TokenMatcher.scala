package com.workday.montague.parser

trait TokenMatcher[T] extends (String => Seq[T])

object AnythingMatcher extends TokenMatcher[String] {
  def apply(str: String): Seq[String] = {
    Seq(str)
  }
}

object IntegerMatcher extends TokenMatcher[Int] {
  def apply(str: String): Seq[Int] = {
    try {
      Seq(Integer.parseInt(str))
    } catch {
      case nfe: NumberFormatException => Nil
    }
  }
}

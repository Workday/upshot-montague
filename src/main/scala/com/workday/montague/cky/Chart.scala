package com.workday.montague.cky

import scala.collection.mutable
import scala.reflect.ClassTag

class Chart[A: ClassTag](parseTokens: IndexedSeq[_]) {
  val n = parseTokens.length

  private[this] val array = Array.ofDim[A](n, n)

  def apply(a: Int, b: Int): A = {
    array(a)(b)
  }

  def diagonal: IndexedSeq[A] = {
    for(i <- 0 until n) yield array(i)(i)
  }

  def allCells: IndexedSeq[(Spans, A)] = {
    for {
      spanLen <- 1 to n
      iStart <- 0 until n - spanLen + 1
      iEnd = iStart + spanLen - 1
    } yield {
      (Spans(iStart, iEnd + 1), array(iStart)(iEnd))
    }
  }

  def update(a: Int, b: Int, c: A): Unit = {
    array(a)(b) = c
  }

  override def toString: String = {
    val sb = new mutable.StringBuilder
    for(spanLen <- 1 to n) {
      sb ++= "spanlen " + spanLen + ":\n"
      for(iStart <- 0 until n - spanLen + 1) {
        val iEnd = iStart + spanLen - 1
        val cell = this(iStart, iEnd)
        val span = Spans(iStart, iEnd + 1)
        val cellSize = cell match { case l: List[_] => l.size case _ => 0 }
        sb ++= s"${span.toString}: ($cellSize entries) ${parseTokens.slice(iStart, iEnd + 1).mkString(" ")}\n"
        sb ++= (cell match {
          case c: List[_] => c.mkString("\n")
          case _ => cell.toString
        })
        sb ++= "\n"
      }
      sb ++= "\n"
    }
    sb.toString()
  }
}

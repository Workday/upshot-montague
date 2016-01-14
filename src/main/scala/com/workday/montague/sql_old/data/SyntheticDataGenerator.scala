package data

import scala.util.Random
import java.util.{Calendar, Date}
import java.sql.{Connection, PreparedStatement}

import scala.reflect.runtime.universe._
import java.text.SimpleDateFormat

class SyntheticDataGenerator(
  tableName: String,
  columnDataGenerators: Map[String, ColumnDataGenerator[_]],
  dependentColumnDataGenerators: Map[String, Map[String, _] => _] = Map()
) {

  def insertData(nRows: Int)(implicit conn: Connection) {
    var questionMarks = columnDataGenerators.values.map { _ match {
      case _: RandomDateGen => "date(?)"
      case _ => "?"
    } }
    questionMarks ++= dependentColumnDataGenerators.values.map(_ => "?")
    val keys = columnDataGenerators.keys.toList ++ dependentColumnDataGenerators.keys // toList is necessary to make the dependent column names come after
    val sql = "insert into " + tableName + " (" + keys.mkString(", ") + ") values (" + questionMarks.mkString(", ") + ")"
    System.out.println(sql)
    val stmt = conn.prepareStatement(sql)
    for(i <- 1 to nRows) {
      insertRow(stmt)
    }
  }

  import scala.collection.mutable

  def insertRow(stmt: PreparedStatement)(implicit conn: Connection) {
    val values = mutable.Map[String, Any]()
    for((cdg, i) <- columnDataGenerators.zipWithIndex) {
      val value = cdg._2.generateValue()
      values += cdg._1 -> value
      setValue(stmt, value, i + 1)
    }
    val n = columnDataGenerators.size
    for((cdg, i) <- dependentColumnDataGenerators.zipWithIndex) {
      val value = cdg._2.apply(values.toMap)
      setValue(stmt, value, n + i + 1)
    }
    stmt.executeUpdate()
  }

  def setValue(stmt: PreparedStatement, value: Any, i: Int) {
    value match {
      case valueStr: String => stmt.setString(i, valueStr)
      case valueNum: Int => stmt.setInt(i, valueNum)
      case valueDate: Date => stmt.setString(i, new SimpleDateFormat("yyyy-MM-dd").format(valueDate))
    }
  }
}

abstract class ColumnDataGenerator[T : TypeTag] {
  protected[this] val rand = new Random()
  def generateValue(): T
}

case class FunctionGen[T : TypeTag](fn: () => T) extends ColumnDataGenerator[T] {
  override def generateValue(): T = fn()
}

case class CategoricalStringGen(values: String*) extends ColumnDataGenerator[String] {
  def generateValue(): String = values(rand.nextInt(values.size))
}

case class NumericRangeGen(low: Int, high: Int) extends ColumnDataGenerator[Int] {
  def generateValue(): Int = rand.nextInt(high - low) + low
}

case class IntIncrementGen(start: Int) extends ColumnDataGenerator[Int] {
  var value = start

  def generateValue(): Int = {
    val ret = value
    value += 1
    ret
  }
}

case class RandomStringGen(lowLen: Int, highLen: Int) extends ColumnDataGenerator[String] {
  def generateValue(): String = {
    val len = rand.nextInt(highLen - lowLen) + lowLen
    var x = new StringBuilder(len)
    for(i <- 1 to len) x += rand.nextPrintableChar()
    x.toString
  }
}

case class RandomArrayElem[T : TypeTag](arr: IndexedSeq[T]) extends ColumnDataGenerator[T] {
  def generateValue(): T = {
    val i = rand.nextInt(arr.length)
    arr(i)
  }
}


case class RandomDateGen(lowDate: Date, highDate: Date) extends ColumnDataGenerator[Date] {
  def generateValue(): Date = {
    var date = Calendar.getInstance()
    date.setTime(lowDate)
    val intervalInDays = ((highDate.getTime() - lowDate.getTime()) / (1000 * 60 * 60 * 24) ).asInstanceOf[Int]
    val days = rand.nextInt(intervalInDays)
    date.add(Calendar.DATE, days)
    //println(DateFormat.getInstance().format(date.getTime))
    date.getTime
  }
}

object RandomDateGen {
  def apply(lowYear: Int, highYear: Int): RandomDateGen = {
    val startDate = Calendar.getInstance()
    startDate.set(lowYear, 0, 1)
    val endDate = Calendar.getInstance()
    endDate.set(highYear, 11, 31)
    RandomDateGen(startDate.getTime, endDate.getTime)
  }
}
package sfdc

import metadata.{Column, Table, Metadata}
import metadata.MetaDictGenerator.{NQColumnInfoImpl, NQTableInfoImpl, NQMetadataImpl}
import sqlkit.{LiteralValue, BinaryPredicate}

/**
 * @todo: Describe this object.
 */
object SfdcNQMetadata {

  def generateNQMetadata(md: Metadata) = {
    new NQSfdcMetadataImpl(md)
  }

  class NQSfdcMetadataImpl(md: Metadata) extends NQMetadataImpl(md, false) {
    override def getTableInfo(tableName: String): Option[NQTableInfoImpl] = md.getTable(tableName).map(new NQSfdcTableInfoImpl(_)(md))
  }

  class NQSfdcTableInfoImpl(table: Table)(implicit md: Metadata) extends NQTableInfoImpl(table, false) {
    override def getColumn(columnName: String): Option[NQColumnInfoImpl] = table.getColumn(columnName).map(new NQSfdcColumnInfoImpl(_))
  }

  class NQSfdcColumnInfoImpl(column: Column)(implicit md: Metadata) extends NQColumnInfoImpl(column) {
    override def generatePredicate(pred: BinaryPredicate): Option[BinaryPredicate] = {
      val superPredOpt = super.generatePredicate(pred)
      superPredOpt map { superPred =>
        // super.generatePredicate() thought it was a valid predicate
        if (column.isStringLike) {
          // Knowing the predicate was valid according to the super, we copy it, drop the upper() and change this to a like
          var bp = pred
          if (bp.op == "contains") {
            // @todo Implement this for the super-class(SQL generation) as well
            bp = bp.copy(op = "like").transformValues(value => LiteralValue("%" + value.asInstanceOf[LiteralValue[String]].value + "%"))
          } else if (bp.op == "=") {
            bp = bp.copy(op = "like")
          } else if (bp.op == "!=") {
            bp = bp.copy(op = "not like")
          }
          Some(bp)
        } else {
          Some(superPred)
        }
      } getOrElse {
        None
      }
    }
  }
}

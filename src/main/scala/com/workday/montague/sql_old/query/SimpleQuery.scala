package query

import sqlkit._
import metadata._
import metadata.Table
import metadata.Column

/**
 * Simple query class generate queries using metadata and sqlkit.
 *
 * To do this all we need to do is provide the typeclass adapting the metadata classes for sqlkit
 */
object SimpleQuery {
  implicit def tableToRelation(table: Table): Relation = BaseRelation(table.fromExpr)
}

// A micro reporting system
sealed trait ReportDef
class TabularReportDef(
  metadata: Metadata,
  masterDetailRelations: Seq[String],
  columns: Seq[String],
  filters: Seq[String]
) extends ReportDef {

  def getSql = {
    val rootTableName = masterDetailRelations(0)
    val rootTableOpt = metadata.getTable(rootTableName)
    rootTableOpt map { rootTable =>
      import SimpleQuery._
      implicit val md = metadata
      val sb = new SelectBuilder(rootTable, "x", rootTableName)
      var parent: Table = rootTable
      var parentAlias = "t"
      for((joinNames, i) <- masterDetailRelations.zipWithIndex.tail) {
        // Canonical join identification:
        val childRelationship = parent.getChildRelationship(joinNames).get
        val table = childRelationship.childTable
        val alias = "t" + i
        val joinExpr = s"${alias}.${childRelationship.childForeignKeyColumn.columnExpr} = ${parentAlias}.id"
        sb.addSqlJoin(SqlJoin(Aliased(BaseRelation(table.fromExpr), alias), joinExpr, InnerJoin), table.fromExpr)
        sb.addProj(StarProj(alias))

        parent = table
        parentAlias = alias
      }
      StandardSqlGenerator.generateSql(sb.toSelect)
    }
  }

  private[this] def translateAliases(map: Map[String, String], expr: String) = {
    var expr2 = expr
    for((t, alias) <- map) {
      expr2 = expr2.replaceAll(t + "\\.", alias + ".")
    }
    expr2
  }
}

import scala.collection.mutable

object Test {
  def main(args: Array[String]) {
    val tables = new mutable.ListBuffer[Table]
    val columns = new mutable.ListBuffer[Column]
    val childRelationships = new mutable.ListBuffer[ChildRelationship]

    val account = new Table("Company", "Account", "Company", Seq("companies"), columns.toList, childRelationships.toList)
    columns.clear()
    columns ++= List(
      Column(account, "Id", "id", "NUMBER", "Id", Nil, PrimaryKeyColumn),
      Column(account, "Name", "name", "VARCHAR", "Name", Nil, SimpleContentInfo[String]),
      Column(account, "Industry", "industry", "VARCHAR", "Industry", Nil, CategorialInfo[String]()),
      Column(account, "NumberOfEmployees", "num_employees", "NUMBER", "Number of employees", List("Headcount"), SimpleContentInfo[Int]))
    account.columns // force lazy eval - should probably introduce a new function

    tables += account

    val dealTable = new Table("Deal", "deal", "Deal", Seq("deals", "leads", "opportunities"), columns.toList)
    val companyColumn = Column(dealTable, "Company", "account_id", "NUMBER", "Company", Nil, ForeignKeyInfo(account))
    columns.clear()
    columns ++= List(
      Column(dealTable, "Id", "id", "NUMBER", "Id", Nil, PrimaryKeyColumn),
      Column(dealTable, "Name", "name", "VARCHAR", "Name", Nil, SimpleContentInfo[String]),
      companyColumn,
      Column(dealTable, "Amount", "amount", "NUMBER", "Amount", List("revenue"), SimpleContentInfo[BigDecimal]),
      Column(dealTable, "Status", "status", "VARCHAR", "Status", Nil, CategorialInfo[String]()),
      Column(dealTable, "IsOpen", "is_open", "VARCHAR", "Is Open", Nil, SimpleContentInfo[Boolean])
    )
    dealTable.columns
    tables += dealTable

    childRelationships += ChildRelationship(dealTable, "Deals", dealTable, companyColumn)
    account.childRelationships // force lazy eval

    val table = new Table("Contact", "contact", "Contact", Seq("contacts"), columns.toList)
    columns.clear()
    columns ++=  List(
      Column(table, "Id", "id", "NUMBER", "Id", Nil, PrimaryKeyColumn),
      Column(table, "Company", "account_id", "NUMBER", "Company", Nil, ForeignKeyInfo(account)),
      Column(table, "FirstName", "first_name", "VARCHAR", "First Name", Nil, SimpleContentInfo[String]),
      Column(table, "LastName", "last_name", "VARCHAR", "Last Name", List("surname"), SimpleContentInfo[String]),
      Column(table, "FullName", "first_name || last_name", "VARCHAR", "Full Name", List("Name"), SimpleContentInfo[String]),
      Column(table, "Title", "title", "VARCHAR", "Title", Nil, SimpleContentInfo[String]),
      Column(table, "Salary", "salary", "NUMBER", "Salary", Nil, SimpleContentInfo[BigDecimal])
    )
    table.columns
    tables += table

    val md = new MetadataImpl(tables.toList)
      /* Seq(
      Table("Company", "account", List(
        Column("Company", "Id", "id", "NUMBER", Datatype.PrimaryKey),
        Column("Company", "Name", "name", "VARCHAR", Datatype.String),
        Column("Company", "Industry", "industry", "VARCHAR", Datatype.String),
        Column("Company", "NumberOfEmployees", "num_employees", "NUMBER", Datatype.Numeric)),
        List(
          ChildRelationship("Company", "Deals", "Deal", "Company"),
          ChildRelationship("Company", "Contacts", "Contact", "Company"))
      ),
      Table("Deal", "deal", List(
        Column("Deal", "Id", "id", "NUMBER", Datatype.PrimaryKey),
        Column("Deal", "Name", "name", "VARCHAR", Datatype.String),
        Column("Deal", "Company", "account_id", "NUMBER", Datatype.ForeignKey("Company")),
        Column("Deal", "Amount", "amount", "NUMBER", Datatype.Numeric),
        Column("Deal", "Status", "status", "VARCHAR", Datatype.String),
        Column("Deal", "IsOpen", "is_open", "VARCHAR", Datatype.Boolean)
      )),
      Table("Contact", "contact", List(
        Column("Contact", "Id", "id", "NUMBER", Datatype.PrimaryKey),
        Column("Contact", "Company", "account_id", "NUMBER", Datatype.ForeignKey("Company")),
        Column("Contact", "FirstName", "first_name", "VARCHAR", Datatype.String),
        Column("Contact", "LastName", "last_name", "VARCHAR", Datatype.String),
        Column("Contact", "FullName", "first_name || last_name", "VARCHAR", Datatype.String),
        Column("Contact", "Title", "title", "VARCHAR", Datatype.String),
        Column("Contact", "Salary", "salary", "NUMBER", Datatype.Numeric)
      )) */

    val report = new TabularReportDef(md, Seq("Company", "Deals"), Nil, Nil)
    System.out.println(report.getSql)
  }

}

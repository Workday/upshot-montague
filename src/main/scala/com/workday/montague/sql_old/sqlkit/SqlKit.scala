import scala.collection.mutable
import sfdc.SoqlParser

/**
 * SqlKit is an immutable SQL representation, generation, and construction library.  It is
 * non-validating and meant for low-level uses.
 *
 * rename to SqlExpr?
 */
package object sqlkit {

  // rename to relationExpr?
  trait Relation

  trait Proj {
    def getSql: String

    override def toString = s"Proj($getSql)"

    /**
     * These methods are super hacky, because they are SOQL-specific. We shouldn't be invoking the SoqlParser at all
     * here. However, they are a necessary workaround until we've evolved proper types (SimpleProjection) that match
     * the SoqlParser rules.
     */
    def isAggregate = SoqlParser.parseAll(SoqlParser.AGGREGATEFIELD, this.getSql).successful
    def isSimple = SoqlParser.parseAll(SoqlParser.tNAME, this.getSql).successful
    def getSqlColumn = {
      assert(isSimple)
      getSql.split("""\.""").last
    }
  }

  case class SelectProj(select: Select) extends Proj {
    override def isAggregate = false
    override def isSimple = false
    override def getSql: String = ???
  }

  trait SortDirection {
    def getSql: String
  }
  case object Asc extends SortDirection {
    override def getSql = "ASC"
  }
  case object Desc extends SortDirection {
    override def getSql = "DESC"
  }

  object StarProj {
    def apply() = new Aliased(new Proj { def getSql = "*" }, None)
    def apply(tableAlias: String) = new Aliased(new Proj { def getSql = tableAlias + ".*"; override def toString = tableAlias + ".*"}, None)
  }

  object Proj {
    def _mkProj(expr: String) =
      new Proj {
        def getSql = expr
        override def toString = s"Proj($expr)"
      }

    def apply(expr: String) = _mkProj(expr)
    def apply(expr: String, alias: String) = new Aliased(_mkProj(expr), Some(alias))
  }

  case class BaseRelation(from: String) extends Relation

  /**
   *
   * @param from
   * @param joins
   * @param projs
   * @param where
   * @param orderBy
   * @param limit
   * @param groupBy
   * @param having
   * @param blobSql An optional blob of SQL, to be appended to the Select statement. This is generated by SoqlParser,
   *                 because we don't do full parsing into SqlKit objects.
   */
  case class Select(from: Aliased[Relation], joins: Seq[Join] = Nil, projs: Seq[Aliased[Proj]],
                    where: Option[Filter] = None, orderBy: Seq[(Proj, Option[SortDirection])] = Nil, limit: Option[Int] = None,
                    groupBy: Seq[Proj] = Nil, having: Option[Filter] = None,
                    blobSql: Option[String] = None) extends Relation {
    val sqlJoins = joins collect { case x: SqlJoin => x }
    val soqlJoins = joins collect { case x: SoqlJoin => x }
    assert(sqlJoins.isEmpty || soqlJoins.isEmpty)   // We currently can't handle both sql and soql joins in the same select clause.
                                                // SqlJoins is when we generate from the parser, SoqlJoins from judged SOQL.

    def getSelectProjs = projs.collect({ case Aliased(s: SelectProj, _) => s})

    def findTableByAlias(tableAlias: String): Option[Aliased[Relation]] = {
      if (from.alias.exists(_ == tableAlias)) {
        Some(from)
      } else {
        // We punt on soqlJoins here
        sqlJoins.find(_.relation.alias.exists(_ == tableAlias)).map(_.relation)
      }
    }

    def fromTableName = from.t.asInstanceOf[BaseRelation].from

    // Map from alias strings to relations, only for SoqlJoins
    private[this] def getAliasToRelationMap = {
      // Warning: Lookup order might be important. Does toMap preserve order?
      val joinAliases = soqlJoins.collect({ case SoqlJoin(Aliased(t, Some(alias))) => (alias -> t) })
      if (from.alias.isDefined)
        (Seq((from.alias.get -> fromTableName)) ++ joinAliases).toMap
      else
        joinAliases.toMap
    }

    /**
     * Expand all aliases detected in s.
     * @warning Only for SoqlJoins
     * @todo Work on List[String] ?
     */
    def expandAliases(pathString: String, aliasToRelationMapOverride: Map[String, String] = Map()): String = {
      val aliasToRelationMap = getAliasToRelationMap ++ aliasToRelationMapOverride
      // Apply aliases to the lookup path in s, and return a string
      def expandAliasesOnce(pathString: String) =
        pathString.split("""\.""").map(lookup => aliasToRelationMap.getOrElse(lookup, lookup)).toList.mkString(".")

      var oldPath = pathString
      var newPath = expandAliasesOnce(oldPath)
      // Replace as many times as is necessary to perform all lookups
      while (oldPath != newPath) {
        oldPath = newPath
        newPath = expandAliasesOnce(oldPath)
      }
      newPath
    }

    def isAggregateQuery = {
      projs.exists(_.t.isAggregate)
    }
  }
  // find out whether it's legal to have order by and group by in same statement.  even then should we allow it?

  /**
   * In SQL statements like "select from", "join", and projections (columns), we might might give the variable
   * an alias name.
   * @todo change this to "trait MaybeAliased" with case classes "Aliased" and "Unaliased"
   * @tparam T Typically Relation, Join, or Projection.
   */
  case class Aliased[+T](t: T, alias: Option[String] = None) {
    /** convert t to a String and if an alias is present, then apply a function to append the alias appropriately */
    def fold(fn1: T => String, fn2: (String, String) => String) = {
      alias.foldLeft(fn1(t))(fn2)
    }

    /**
     * Suppose this alias represents a table alias, say "t".  Then return the column name qualified by this table alias
     * e.g. "t.ColumnName".  If there is no alias then return simply "ColumnName".
     *
     * @return the columnName qualified by this table alias, unless there is no table alias
     */
    def dotted(columnName: String) = {
      alias.map(_ + ".").getOrElse("") + columnName
    }
  }
  object Aliased { def apply[T](t: T, alias: String): Aliased[T] = Aliased(t, Some(alias)) }

  trait Join
  case class SoqlJoin(relation: Aliased[String]) extends Join   // e.g. 'FROM Contact c, c.Account a'
  case class SqlJoin(relation: Aliased[Relation], joinExpr: String, joinType: JoinType) extends Join
  sealed trait JoinType
  case object InnerJoin extends JoinType
  case object LeftOuterJoin extends JoinType
  case object RightOuterJoin extends JoinType
  case object FullOuterJoin extends JoinType

  case class Filter(root: FilterExpr)
  trait FilterExpr {
    def getFilterSql: String = ???
    def transformColumnSql(fn: String => String): FilterExpr = ???
  }

  case class FilterPred(expr: String) extends FilterExpr {
    override def getFilterSql: String = expr

    override def transformColumnSql(fn: String => String): FilterExpr = new FilterPred(fn(expr))
  }

  case class LazyFilterPred(expr: SqlStringGenerator => String) extends FilterExpr {

  }

  trait Value

  case class BinaryPredicate(column: String, op: String, values: List[Value]) extends FilterExpr {
    override def transformColumnSql(fn: (String) => String): BinaryPredicate = this.copy(column = fn(column))
    def transformValues(fn: Value => Value): BinaryPredicate = this.copy(values = values.map(fn))
  }

  // @todo Create PicklistValue and NumericalValue
  // https://app.asana.com/0/8587138393984/8662823801306
  case class LiteralValue[T](value: T) extends Value
  trait DateLikeValue extends Value

  /**
   * Extend our basic SQL AST with support for variables.  Variables are placeholders with values that can be
   * substituted when the AST is converted to a string.  In order to support these you must mix in the
   * SqlVariableGenerator trait when instantiating your SqlStringGenerator.
   */
  case class SqlVariable(name: String) extends sqlkit.Value

  /**
   * Represent relative date periods like "last month" or "last 2 years".  These date periods are relative to today and are specified with a
   * granularity (date, month, year, etc.) and a starting offset.
   */
  case class RelativeDateRangeValue(gran: DateGranularity, relativeStart: Int) extends DateLikeValue
  /**
   * Represent absolute dates like "2013".  Only supports dateGranularity = Year right now.
   * TODO: more thought should be put into this.  potentially we could use joda-time Partials
   * @param value the value for the specified granularity.  For example, when gran = Year, value is the year.
   */
  case class AbsoluteDatePeriodValue(gran: DateGranularity, value: Int) extends DateLikeValue
  case class FuncValue(func: String, arg: Value) extends Value

  case class AndFilter(children: List[FilterExpr]) extends FilterExpr

  sealed trait DateGranularity {
    def sql = this.getClass.getSimpleName.filter(_ != '$').toLowerCase
    def uppercaseSoql = this.getClass.getSimpleName.filter(_ != '$').toUpperCase
  }
  case object Day extends DateGranularity
  case object Week extends DateGranularity
  case object Month extends DateGranularity
  case object Quarter extends DateGranularity
  case object Year extends DateGranularity
  // We use underscores so that .sql matches SF date formats:
  // http://www.salesforce.com/us/developer/docs/officetoolkit/Content/sforce_api_calls_soql_select_dateformats.htm
  case object Fiscal_Quarter extends DateGranularity
  case object Fiscal_Year extends DateGranularity

  // We should allow expressions in more places than at salesforce:
  // "companies with employees larger than the average"
  //case class Predicate(proj: Proj, operator: Operator, )

  /**
   * @param fromTableName is a convenient (durable) way for the client to refer to the main from table.  SelectBuilder
   *                      keeps track of aliases for the client's convenience but doesn't use these names otherwise.
   */
  class SelectBuilder(from: Relation, val alias: String, val fromTableName: String) {
    private[this] val joins = new mutable.ListBuffer[Join]
    private[this] val projs = new mutable.ListBuffer[Aliased[Proj]]
    private[this] var filter = new mutable.ListBuffer[FilterExpr]
    private[this] var limit: Option[Int] = None
    private[this] val orderBy = new mutable.ListBuffer[(Proj, Option[SortDirection])]
    private[this] val groupBy = new mutable.ListBuffer[Proj]
    private[this] val blobSql = None
    //private[this] val filter: FilterExpr = ???

    private[this] val tablesToAliases = new mutable.HashMap[String, String]

    tablesToAliases += (fromTableName -> alias)

    def getAlias(tableName: String): Option[String] = {
      tablesToAliases.get(tableName)
    }

    // we could put everything behind an overloaded += syntax
    def addSqlJoin(sqlJoin: SqlJoin, tableName: String) = {
      joins += sqlJoin
      tablesToAliases += (tableName -> sqlJoin.relation.alias.get)
    }

    def clearProjs() = projs.clear()

    def addProj(proj: Aliased[Proj]) = projs += proj

    def addFilterConj(pred: FilterExpr) = this.filter += pred

    def setLimit(n: Int) = this.limit = Some(n)

    def addOrderBy(proj: Proj, sortDirection: Option[SortDirection]) = this.orderBy += ((proj, sortDirection))

    def addGroupBy(expr: String, addToSelect: Boolean) = {
      val proj = new Proj { def getSql = expr }
      this.groupBy += proj
      if (addToSelect) this.projs += Aliased(proj)
    }

    // @todo Get rid of the tableName param since you can get it from the relation
    def innerJoin(select: Select, joinExprStr: String, tableName: String) = {
      addSqlJoin(SqlJoin(select.from, joinExprStr, InnerJoin), tableName)
      for(join <- select.sqlJoins) {
        val joinFrom = select.findTableByAlias(join.relation.alias.get).get.t match {
          case s: Select => s.from.t.asInstanceOf[BaseRelation].from
          case b: BaseRelation => b.from
        }
        addSqlJoin(join, joinFrom)
      }
      for(proj <- select.projs) addProj(proj)
      select.where match {
        case Some(Filter(AndFilter(filterList))) => for(filter <- filterList) addFilterConj(filter)
        case Some(_) => throw new UnsupportedOperationException("only AndFilters are supported for inner joins")
        case None => // do nothing
        // otherwise throw exception
      }
    }

    def leftOuterJoin(select: Select, alias: String, joinExprStr: String, tableName: String) = {
      addSqlJoin(SqlJoin(Aliased(select, Some(alias)), joinExprStr, LeftOuterJoin), tableName)
      for(proj <- select.projs) addProj(proj)
    }

    def toSelect: Select = {
      val filterOpt = if (this.filter.isEmpty) None else Some(Filter(AndFilter(this.filter.toList)))
      Select(Aliased(from, alias), joins, projs, filterOpt, this.orderBy, this.limit, this.groupBy, None, blobSql)
    }
  }

  import scala.reflect.runtime.universe._

  trait SqlStringGenerator {
    def generateSql(select: Select): String
  }

  /**
   * Mix in this trait if you want to support SqlVariables
   */
  trait SqlVariableGenerator extends BaseSqlGenerator {

    def variableValue(variableName: String): LiteralValue[_]

    override def generateSqlPart[T : TypeTag](a: T): String = {
      a match {
        case SqlVariable(name) => super.generateSqlPart(variableValue(name))
        case _ => super.generateSqlPart(a)
      }
    }
  }

  /**
   * Keep in mind we may want/need to implement different sql generators in the future
   * to support various SQL databases (e.g. Oracle)
   */
  class BaseSqlGenerator extends SqlStringGenerator {

    def generateSql(select: Select): String = generateSqlPart[Select](select)

    // Recursive pattern match. References to understand this idiom:
    /* See http://stackoverflow.com/questions/8618082/visitor-pattern-in-scala
     * See:
     * http://stackoverflow.com/questions/16056645/how-to-pattern-match-on-generic-type-in-scala?rq=1
     * http://stackoverflow.com/questions/12218641/scala-2-10-what-is-a-typetag-and-how-do-i-use-it
     * https://issues.scala-lang.org/browse/SI-6517
     */
    protected def generateSqlPart[T : TypeTag](a: T): String =
      a match {
        case Select(from, joins, projs, where, orderBy, limit, groupBy, having, blobSql) =>
          "SELECT " + projs.map(generateSqlPart(_)).mkString(", ") + "\n" +
          "FROM " + generateSqlPart(from) + "\n" +
          joins.map(generateSqlPart(_)).convertOrElse(_.mkString("\n") + "\n", "")  +
          where.map(generateSqlPart(_)).map(_ + "\n").getOrElse("") +
          orderBy.convertOrElse("ORDER BY " + generateSqlPart(_) + "\n", "") +
          groupBy.convertOrElse("GROUP BY " + generateSqlPart(_) + "\n", "") +
          limit.map("LIMIT " + _ + "\n").getOrElse("") +
          blobSql.getOrElse("")
        case a: Seq[(Proj, Option[SortDirection]) @unchecked] if typeOf[T] =:= typeOf[Seq[(Proj, Option[SortDirection])]] =>
          a.map(x => {
            generateSqlPart(x._1) + x._2.map(" " + _.getSql).getOrElse("")
          }).mkString(", ")
        case a: Aliased[Proj @unchecked] if typeOf[T] =:= typeOf[Aliased[Proj]] => a.fold(generateSqlPart(_), _ + " AS " + _)
        case a: Aliased[Relation @unchecked] if typeOf[T] =:= typeOf[Aliased[Relation]] => a.fold(generateFrom(_), _ + " " + _)
        case a: Aliased[SqlJoin @unchecked] if typeOf[T] =:= typeOf[Aliased[SqlJoin]] => a.fold(generateSqlPart(_), _ + " " + _)
        case BaseRelation(from) => from
        case join: SqlJoin => joinTypeSql(join.joinType) + " " + generateSqlPart(join.relation) + " ON " + join.joinExpr
        case join: SoqlJoin => ???
        case projs: Seq[Proj @unchecked] if typeOf[T] =:= typeOf[Seq[Proj]] => projs.map(generateSqlPart(_)).mkString(", ")
        case Filter(expr) => "WHERE " + generateSqlPart(expr)
        case FilterPred(pred) => pred
        case BinaryPredicate(column, op, List((dateRange: RelativeDateRangeValue))) => column + " >= date('now', 'start of year') AND " + column + " < date('now', 'start of year', '+1 year')"
        case AndFilter(children) => children.map("(" + generateSqlPart(_) + ")").mkString(" AND ")
        case BinaryPredicate(column, op, List(lit)) => column + " " + op + " " + generateSqlPart(lit)
        case LiteralValue(numValue: Number) => numValue.toString
        case LiteralValue(strValue: String) => "'" + strValue + "'"
        case FuncValue(func, value) => func + "(" + generateSqlPart(value) + ")"
        case LazyFilterPred(expr) => expr(this)
        case SelectProj(select) => "(" + generateSqlPart(select) + ")"
        case proj: Proj => proj.getSql
      }

    def generateFrom(rel: Relation): String = {
      rel match {
        case BaseRelation(from) => from
        case sel: Select => "(" + generateSqlPart(sel) + ")"
      }
    }

    implicit class RichList[A, M[A] <: Iterable[A]](list: M[A]) {
      def convertOrElse[B](fn: M[A] => B, whenEmpty: B) = {
        if (!list.isEmpty) fn(list) else whenEmpty
      }
    }

    def joinTypeSql(joinType: JoinType) = {
      joinType match {
        case InnerJoin => "join"
        case LeftOuterJoin => "left outer join"
        case RightOuterJoin => "right outer join"
        case FullOuterJoin => "full outer join"
      }
    }

    def main(args: Array[String]) {
      val select = Select(Aliased(BaseRelation("account"), "a"), Seq(SqlJoin(Aliased(BaseRelation("contact"), "c"), "c.account_id = a.id", InnerJoin)),
        Seq(StarProj("a"), StarProj("c")))
      System.out.println(generateSql(select))
    }

  }

  // Used with sqlite
  object StandardSqlGenerator extends BaseSqlGenerator {
    override protected def generateSqlPart[T: TypeTag](a: T): String = {
      a match {
        case BinaryPredicate(column, op, List((RelativeDateRangeValue(gran, n)))) =>
          val (startOffset, endOffset) =
            if (n < 0) {
              (n, 0)
            } else if (n == 0) {
              (0, 1)
            } else if (n > 0) {
              (1, n + 1)
            }
          s"$column >= date('now', 'start of ${gran.sql}', '$startOffset ${gran.sql}') AND $column < date('now', 'start of ${gran.sql}', '${endOffset} ${gran.sql}')"
        case _ => super.generateSqlPart(a)
      }
    }
  }

  // Used with Postgres
  object AnsiSqlGenerator extends BaseSqlGenerator {
    override protected def generateSqlPart[T: TypeTag](a: T): String = {
      a match {
        case BinaryPredicate(column, op, List((RelativeDateRangeValue(gran, n)))) =>
          val (startOffset, endOffset) =
            if (n < 0) {
              (n, 0)
            } else if (n == 0) {
              (0, 1)
            } else if (n > 0) {
              (1, n + 1)
            }
          s"$column >= date_trunc('${gran.sql}', now()) + interval '$startOffset ${gran.sql}' AND $column < date_trunc('${gran.sql}', now()) + interval '${endOffset} ${gran.sql}'"
        case _ => super.generateSqlPart(a)
      }
    }
  }

}

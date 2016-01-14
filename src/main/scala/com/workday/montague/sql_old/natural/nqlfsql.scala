package natural

/**
 * Sql generation
 */
import sqlkit._

/**
 * This class is a bit of a misnomer because this generates a _logical_ representation of the SQL, which you then
 * need to turn into physical SQL (using say the StandardSqlGenerator further below).
 */
class SqlGen(metadata: NQMetadata, columnChooser: ColumnSelector = StarColumnsSelector) {
  // Generate Select statements *as well as* the way of rendering the rows.
  def generate(lf: LFNode): Option[Select] = {
    implicit val metadataImp = metadata
    val sb = lf match {
      case r: RelationLike =>
        val sbOpt = generatePart(r, "t")
        sbOpt
      case c: ColumnLike => columnToRelation(c) flatMap { generatePart(_, "t") }
      case _ => None
    }
    sb map { selectBuilder =>
      val select = selectBuilder.toSelect
      select
    }
  }

  private[this] def selectDisplayColumns(sb: SelectBuilder, tableName: String, tableAlias: String)(implicit md: NQMetadata): Option[SelectBuilder] = {
    val displayColumns = columnChooser.getSqlColumnsFor(tableName, "")
    if (!displayColumns.isEmpty) {
      for(displayColumn <- displayColumns) {
        sb.addProj(Aliased(Proj(tableAlias + "." + displayColumn)))
      }
      Some(sb)
    } else {
      None
    }
  }

  // Non-exhaustive transformation allows us to build up the generator gradually, while still successfully parsing a lot of things that
  // we don't yet handle
  // i.e. it lets us cheat majorly
  private[this] def generatePart(lf: LFNode, relationAlias: String)(implicit metadata: NQMetadata): Option[SelectBuilder] = {
    lf match {
      case BaseRelation(tableName) =>
        val select = new SelectBuilder(new sqlkit.BaseRelation(getFromExpr(tableName)), relationAlias, tableName)
        selectDisplayColumns(select, tableName, relationAlias) getOrElse select.addProj(StarProj(relationAlias))
        Some(select)
      case Filter(relation, DynamicPredicate(generatePredicateFn)) =>
        generatePredicateFn(relation) flatMap { basePredicate =>
          // substitute Filter(relation, basePredicate) for this filter and try generation again
          generatePart(Filter(relation, basePredicate), relationAlias)
        }
      case Filter(relation, BasePredicate(predicateTableName, predicate)) =>
        generatePart(relation, relationAlias) flatMap { select =>
          if (predicateTableName == "_" || SqlGen.findUnderlyingTableName(relation).map(_ == predicateTableName).getOrElse(false)) {
            var newPredicate = predicate.transformColumnSql(joinAndSubstColumnPath(select, _))
            val tableName = select.fromTableName
            newPredicate = newPredicate.transformColumnSql(substProminentColumns(tableName, _))
            newPredicate = newPredicate.transformColumnSql(substituteAliases(Map("t" -> relationAlias), _))
            newPredicate match {
              case bp: BinaryPredicate => generatePredicates(tableName, bp) match {
                case Some(bp2) =>
                  select.addFilterConj(bp2)
                  Some(select)
                case None => None
              }
              case _ => // do nothing
                select.addFilterConj(newPredicate)
                Some(select)
            }
          } else {
            None
          }
        }
      case AggregateBy(ColumnOp(op, BaseColumn(tableName, columnName, _)), groupingOpt) =>
        val groupingOpt2 = groupingOpt map { grouping =>
          substProminentColumns(BaseRelation(tableName), grouping, metadata) match {
            case Some(column) => column
            case None =>
              // We had a grouping, but substProminentColumns() failed, so this entire case should return None
              // meaning no valid SQL could be generated for this logical form
              // Note that this return exits generatePart(), http://stackoverflow.com/questions/3770989/purpose-of-return-statement-in-scala
              // which is what we want (rather than returning a value for this particular closure)
              // (In later instances we abbreviate this entire match block to `getOrElse(return None)`
              return None
          }
        }
        val rel = groupingOpt2 match {
          case Some(ColumnOf(rel, columnName)) => rel
          case _ => BaseRelation(tableName)
        }
        generatePart(rel, relationAlias) flatMap {
          select =>
            select.clearProjs()
            select.addProj(Proj(op + "(" + relationAlias + "." + columnName + ")", "agg"))
            addGrouping(select, rel, groupingOpt2)
        }
      case AggregateBy(ColumnOp(op, ColumnOf(relation, columnName)), groupingOpt) =>
        // See comment above for an explanation of the `return None`
        val groupingOpt2 = groupingOpt.map(substProminentColumns(relation, _, metadata).getOrElse(return None))
        val commonRelationOpt = groupingOpt2 match {
          case Some(ColumnOf(groupingRelation, _)) =>
            transposeFilters(relation, groupingRelation)
          case Some(x: BaseColumn) => Some(relation)
          case Some(_) => None // unsupported
          case None => Some(relation)
        }
        commonRelationOpt flatMap { commonRelation =>
          generatePart(commonRelation, relationAlias) flatMap { select =>
            select.clearProjs()
            if (!columnName.contains('.')) {
              select.addProj(Proj(op + "(" + relationAlias + "." + columnName + ")", "agg"))
            } else {
              select.addProj(Proj(op + "(" + columnName + ")", "agg"))
            }
            addGrouping(select, commonRelation, groupingOpt2)
          }
        }
      case AggregateBy(ColumnOp("count", DummyColumn(relation)), groupingOpt) =>
        // See comment above for an explanation of the `return None`
        val groupingOpt2 = groupingOpt.map(substProminentColumns(relation, _, metadata).getOrElse(return None))
        generatePart(relation, relationAlias) flatMap { select =>
          // the only time we need to snip this off and wrap it is when the grouping column is ...
          SqlGen.findUnderlyingTableName(relation) flatMap { tableName =>
            metadata.getTableInfo(tableName) flatMap { table =>
              select.clearProjs()
              select.addProj(Proj("count(" + relationAlias + "." + table.countProj + ")", "cnt"))
              addGrouping(select, relation, groupingOpt2)
            }
          }
        }
      case LimitBy(n, relation, sortColumn: BaseColumn, sortDir) =>
        generatePart(relation, relationAlias) flatMap { select =>
          substProminentColumns(relation, sortColumn, metadata) map { sortColumn =>
            select.setLimit(n)
            // TODO: make the sort direction smarter
            select.addOrderBy(Proj(relationAlias + "." + sortColumn.columnName), Some(sortDir))
            select
          }
        }
      case OrganizeBy(relation, sortColumn: BaseColumn, sortDirection) =>
        // TODO: maybe we should do sort by -amount etc instead of asc/desc
        generatePart(relation, relationAlias) flatMap { select =>
          substProminentColumns(relation, sortColumn, metadata) flatMap { sortColumn =>
            findOrJoinNecessaryTables(select, sortColumn) flatMap { sortColumnTableAlias =>
              if (!sortColumn.columnName.contains('.')) {
                select.addOrderBy(Proj(sortColumnTableAlias + "." + sortColumn.columnName), Some(sortDirection))
              } else {
                select.addOrderBy(Proj(sortColumn.columnName), Some(sortDirection))
              }
              Some(select)
            }
          }
        }
      case Relate(rel1, rel2, InnerJoin) =>
        // BUGBUG:  If you do "biotechnology accounts with open opportunities" you get "biotechnology (accounts with open opportunities)" and the
        // outer Filter() expects a relation with alias "t" (and it must be the accounts table)
        // we might be able to live with this for now
        generatePart(rel1, relationAlias) flatMap { select =>
          generatePart(rel2, relationAlias + "2") flatMap { select2 =>
            metadata.getJoinInfo(select.fromTableName -> select2.fromTableName) map { joinSql =>
              val joinSql2 = substituteAliases(Map("from" -> select.alias, "to" -> select2.alias), joinSql)
              select.innerJoin(select2.toSelect, joinSql2, select2.fromTableName)
              select
            }
          }
        }
      case Relate(rel1, rel2, OuterJoin) =>
        // BUGBUG:  If you do "biotechnology accounts with open opportunities" you get "biotechnology (accounts with open opportunities)" and the
        // outer Filter() expects a relation with alias "t" (and it must be the accounts table)
        // we might be able to live with this for nows
        generatePart(rel1, relationAlias) flatMap { select =>
          generatePart(rel2, relationAlias + "2") flatMap { select2 =>
            metadata.getJoinInfo(select.fromTableName -> select2.fromTableName) map { joinSql =>
              val joinSql2 = substituteAliases(Map("from" -> select.alias, "to" -> (relationAlias + "2")), joinSql)
              select.leftOuterJoin(select2.toSelect, relationAlias + "2", joinSql2, select2.fromTableName)
              select
            }
          }
        }
      case Relate(rel, sub, AntiJoin) =>
        generatePart(rel, relationAlias) flatMap { select =>
          val subalias = relationAlias + "_sub"
          generatePart(sub, subalias) flatMap { subselect =>
            metadata.getJoinInfo(subselect.fromTableName -> select.fromTableName) map { joinSql =>
              subselect.clearProjs()
              subselect.addProj(Aliased(Proj("1")))
              // Correlated subquery
              val joinSql2 = substituteAliases(Map("from" -> subselect.alias, "to" -> select.alias), joinSql)
              subselect.addFilterConj(new FilterPred(joinSql2))
              select.addFilterConj(new LazyFilterPred(gen => "not exists (" + gen.generateSql(subselect.toSelect) + ")"))
              select
            }
          }
        }
      case _ =>
        None
    }
  }

  private[this] def getFromExpr(tableName: String)(implicit metadata: NQMetadata): String = {
    metadata.getTableInfo(tableName).map(_.fromExpr).getOrElse(tableName)
  }

  private[this] def generatePredicates(tableName: String, pred: BinaryPredicate)(implicit metadata: NQMetadata): Option[BinaryPredicate] = {
    metadata.getTableInfo(tableName) flatMap { table =>
      SqlGen.extractColumnReferences(pred.column).headOption flatMap { tableAliasAndColumn =>
        val columnName = tableAliasAndColumn._2
        table.getColumn(columnName) map { column =>
          val pred2 = column.generatePredicate(pred)
          pred2
        }
      }
    } getOrElse Some(pred)
  }

  /**
   * Unwrap each Filter predicate from relation r1 and apply it to r2, recursively.  This is for constructions like
   * "total wellness deal revenue per month last year"
   *
   * which parses to AggregateBy(column1, column2)
   *
   * with column1 = "total wellness deal revenue" and column2 = "last year deals month"
   *
   * so both columns refer to the deals table independently and have separate filters on the deals table (wellness
   * deals and last year's deals, respectively).
   *
   * TODO: we might be able to handle this better by changing AggregateBy() so that it has an "underlyingTable"
   * parameter and column1 and column2 are always columns of that underlying table.  But it's not obvious at the moment
   * how that works when column1 and column2 refer to say different tables.
   *
   * So anyway if we restrict column1 and column2 to refer to the same underlying table, then we can consolidate
   * the two tables by taking the filters from one and wrapping them around the other, using this function.
   */
  private[this] def transposeFilters(r1: RelationLike, r2: RelationLike) = {
    def transposeFiltersRecursive(r1: RelationLike, r2: RelationLike): Option[RelationLike] = {
      r1 match {
        case Filter(rel, pred) =>
          val r3 = Filter(r2, pred)
          transposeFiltersRecursive(rel, r3)
        case _: BaseRelation =>
          Some(r2)
        case _ => None
      }
    }

    SqlGen.findUnderlyingTableName(r1) flatMap { r1TableName =>
      SqlGen.findUnderlyingTableName(r2) flatMap { r2TableName =>
        if (r1TableName == r2TableName) {
          // We could potentially try going both directions
          transposeFiltersRecursive(r1, r2)
        } else {
          None
        }
      }
    }
  }

  private[this] def substProminentColumns[C <: ColumnLike](rel: RelationLike, originalColumn: C, metadata: NQMetadata): Option[C] = {
    SqlGen.findUnderlyingTableName(rel) flatMap { tableName =>
      substProminentColumns(tableName, originalColumn, metadata)
    }
  }

  /**
   * @return None when originalColumn asks for $prominentNumericColumn but none can be found
   *         Some(new column) when originalColumn asks for $prominentNumericColumn and we find a suitable match
   *         Some(originalColumn) if originalColumn doesn't contain any substitutions
   */
  private[this] def substProminentColumns[C <: ColumnLike](tableName: String, originalColumn: C, metadata: NQMetadata): Option[C] = {
    // TODO: get rid of BaseColumn and always formulate it as ColumnOf(BaseRelation(table), BaseColumn(table, column)) or something like that?
    originalColumn match {
      case BaseColumn("_", columnExpr, _) if columnExpr.contains("$prominentNumericColumn") => {
        for(tableInfo <- metadata.getTableInfo(tableName);
            prominentNumericColumn <- tableInfo.prominentNumericColumnOpt)
        yield BaseColumn(tableName, columnExpr.replaceAllLiterally("$prominentNumericColumn", prominentNumericColumn), false).asInstanceOf[C]
      }
      case ColumnOf(rel, columnExpr) if columnExpr.contains("$prominentNumericColumn") => {
        for(tableInfo <- metadata.getTableInfo(tableName);
            prominentNumericColumn <- tableInfo.prominentNumericColumnOpt)
        yield ColumnOf(rel, columnExpr.replaceAllLiterally("$prominentNumericColumn", prominentNumericColumn)).asInstanceOf[C]
      }
      case BaseColumn("_", columnExpr, _) if columnExpr.contains("$prominentDateColumn") => {
        for(tableInfo <- metadata.getTableInfo(tableName);
            prominentDateColumn <- tableInfo.prominentDateColumn)
        yield BaseColumn(tableName, columnExpr.replaceAllLiterally("$prominentDateColumn", prominentDateColumn), false).asInstanceOf[C]
      }
      case ColumnOf(rel, columnExpr) if columnExpr.contains("$prominentDateColumn") => {
        for(tableInfo <- metadata.getTableInfo(tableName);
            prominentDateColumn <- tableInfo.prominentDateColumn)
        yield ColumnOf(rel, columnExpr.replaceAllLiterally("$prominentDateColumn", prominentDateColumn)).asInstanceOf[C]
      }
      case _ => Some(originalColumn)
    }
  }

  private[this] def substProminentColumns(tableName: String, columnExpr: String)(implicit metadata: NQMetadata): String = {
    // TODO: get rid of BaseColumn and always formulate it as ColumnOf(BaseRelation(table), BaseColumn(table, column)) or something like that?
    var newColumnExpr = columnExpr
    if (newColumnExpr.contains("$prominentNumericColumn")) {
      for(tableInfo <- metadata.getTableInfo(tableName);
          prominentNumericColumn <- tableInfo.prominentNumericColumnOpt)
        newColumnExpr = newColumnExpr.replaceAllLiterally("$prominentNumericColumn", prominentNumericColumn)
    }
    if (newColumnExpr.contains("$prominentDateColumn")) {
      for(tableInfo <- metadata.getTableInfo(tableName);
          prominentDateColumn <- tableInfo.prominentDateColumn)
        newColumnExpr = newColumnExpr.replaceAllLiterally("$prominentDateColumn", prominentDateColumn)
    }
    newColumnExpr
  }

  private[this] def addGrouping(select: SelectBuilder, relation: RelationLike, groupingOpt: Option[ColumnLike])(implicit metadata: NQMetadata): Option[SelectBuilder] = {
    groupingOpt match {
      case Some(col) =>
        findUnderlyingBaseColumn(col) flatMap { baseColumn =>
          findOrJoinNecessaryTables(select, col) flatMap { colTableAlias =>
          // HACKHACK:  If it doesn't contain a dot then it's a simple column reference.  If it does contain a dot, then it's a column expression.
            if (!baseColumn.columnName.contains('.')) {
              select.addGroupBy(colTableAlias + "." + baseColumn.columnName, true)
            } else {
              select.addGroupBy(baseColumn.columnName, true)
            }
            Some(select)
          }
        }
      case None => Some(select) // unmodified
    }
  }

  /**
   * @return the alias of the table for the column, or None if there is no applicable table
   */
  private[this] def findOrJoinNecessaryTables(select: SelectBuilder, column: ColumnLike)(implicit metadata: NQMetadata): Option[String] = {
    findUnderlyingTableName(column) flatMap { tableName =>
      val alias = select.getAlias(tableName)
      alias orElse {
        val shouldSearchForJoin =
        /**
         * Only search for the join if this reference to the column was explicitly "qualified" with the name of the table for
         * the column.  I.e. allow searching for "opportunities by account name" but not "opportunities by name"
         * Note that "opportunities by industry" still works using the star parser.
         */
          column match {
            case BaseColumn(_, _, isQualified) => isQualified
            case _ => true
          }
        if (shouldSearchForJoin) {
            // This code isn't adequate at all for example it doesn't handle more than 1-step traversals, but it will have to do for now
            // it doesn't try hard enough to create new aliases, etc.
            metadata.getJoinInfo(select.fromTableName -> tableName) map {
              joinSql =>
                val newAlias = tableName.charAt(1).toString
                val joinSql2 = substituteAliases(Map("from" -> select.alias, "to" -> newAlias), joinSql)
                select.addSqlJoin(sqlkit.SqlJoin(Aliased(sqlkit.BaseRelation(tableName), newAlias), joinSql2, sqlkit.InnerJoin), tableName)
                newAlias
            }
        } else {
          None
        }
      }
    }
  }

  // HACKHACK for tagged
  private[this] def joinAndSubstColumnPath(select: SelectBuilder, columnPathExpr: String)(implicit metadata: NQMetadata): String = {
    var newColumnPath = columnPathExpr
    if (newColumnPath.contains("t.user_id.")) {
      val tableName = "userdata_light"
      val newAlias = "udl_from"
      select.addSqlJoin(sqlkit.SqlJoin(Aliased(sqlkit.BaseRelation(tableName), newAlias), "udl_from.user_id = t.user_id", sqlkit.InnerJoin), "t.user_id")
      newColumnPath = substituteAliases(Map("t.user_id" -> newAlias), newColumnPath)
    }
    if (newColumnPath.contains("t.recipient_user_id.")) {
      val tableName = "userdata_light"
      val newAlias = "udl_to"
      select.addSqlJoin(sqlkit.SqlJoin(Aliased(sqlkit.BaseRelation(tableName), newAlias), "udl_to.user_id = t.recipient_user_id", sqlkit.InnerJoin), "t.recipient_user_id")
      newColumnPath = substituteAliases(Map("t.recipient_user_id" -> newAlias), newColumnPath)
    }
    newColumnPath
  }

  private[this] def findUnderlyingTableName(column: ColumnLike): Option[String] = {
    column match {
      case BaseColumn(tableName, _, _) => Some(tableName)
      case ColumnOf(rel, _) => SqlGen.findUnderlyingTableName(rel)
      case _ => None
    }
  }

  private[this] def findUnderlyingBaseColumn(column: ColumnLike): Option[BaseColumn] = {
    column match {
      case BaseColumn(tableName, columnName, isQualified) => Some(BaseColumn(tableName, columnName, isQualified))
      case ColumnOf(rel, columnName) => SqlGen.findUnderlyingTableName(rel) map { BaseColumn(_, columnName, true) }
      case _ => None
    }
  }

  private[this] def substituteAliases(map: Map[String, String], str: String) = {
    var str2 = str
    for((key, value) <- map) {
      str2 = str2.replaceAllLiterally(key + ".", value + ".")
    }
    str2
  }

  // "deals by quarter" because quarter is categorial, we'll do a summary report.  and summary reports always show summaries as well?

  // option grouping is kind of a flop because the only time it's None is when i coerce a column expression to a relation

  private[this] def columnToRelation(column: ColumnLike): Option[RelationLike] = {
    column match {
      case cop@ColumnOp(op, _) => Some(AggregateBy(cop, None))
      case _ => None
    }
  }

  // TODO: columnToColumnOp (rename to AggColumn)
}

object SqlGen {
  // crude system until we do a formal sql++ type thing
  // rename to findPrimaryUnderlyingTableName?
  def findUnderlyingTableName(rel: RelationLike): Option[String] = {
    rel match {
      case Filter(rel, pred) => findUnderlyingTableName(rel)
      case BaseRelation(tableName) => Some(tableName)
      case OrganizeBy(rel, column, sortDirection) => findUnderlyingTableName(rel)
      case AggregateBy(op, grouping) => None
      case Limit(n, rel) => findUnderlyingTableName(rel)
      case LimitBy(n, rel, sort, _) => findUnderlyingTableName(rel)
      case Relate(rel, join, joinType) => findUnderlyingTableName(rel)
    }
  }

  /**
   * Given a column expression like "(t.Amount + t.NumberOfEmployees) < 73" this function finds all
   * the table aliases and and column references and returns them as a list of (tableAlias, columnName) pairs
   */
  def extractColumnReferences(columnExpr: String) = {
    // @todo The second (\w+) should be expanded to digits and underscores at least to capture things like "t.CustomField3__c"
    val regex = """(\w+)\.(\w+)""".r
    val refs = for(regex(tableAlias, columnName) <- regex.findAllIn(columnExpr))
                yield (tableAlias, columnName)
    refs.toList
  }

}

package natural

import sqlkit.{DateLikeValue, RelativeDateRangeValue, LiteralValue, BinaryPredicate}

/**
 * Helper methods for dictionary generation
 */
trait DictHelpers {
  protected def filterByValue(tableName: String, column: String, values: Seq[String]) = {
    // first value is the value, the rest are synonyms
    val value = values.head
    // Generates a predicate
    // @todo For a boolean, do something different. We need different SQL
    // @todo Why are we hard generating SQL here? This is a pain because we have physical SQL at the logical SQL level,
    //  thus making it hard to switch QLs (e.g. to SOQL). So we should generate BinaryPredicates.
    values -> (adjectiveFilter(tableName, "t." + column + " = '" + value + "'") ++: adjectiveColumnFilter(tableName, column, values) ++: valueLiteral(value))
  }
  protected def filterByBooleanValue(tableName: String, column: String, value: Boolean, tokens: Seq[String]) = {
    // first value is the value, the rest are synonyms
    // @todo Does it really make sense that Boolean are Categorials that have "string" values?
    // Generates a predicate
    // @todo Why are we hard generating SQL here? This is a pain because we have physical SQL at the logical SQL level,
    //  thus making it hard to switch QLs (e.g. to SOQL). So we should generate BinaryPredicates.
    tokens -> adjectiveFilter(tableName, "t." + column + " = " + value + "")
  }

  private[this] def valueLiteral(value: String) = {
    List(
      (Noun, Done(Literal(value)))
    )
  }

  protected def numericColumn(tableName: String, columnName: String, names: Seq[String], defaultOp: Option[String] = None) = {
    var x = column(tableName, columnName, names)
    if (defaultOp.isDefined) {
      x = x._1 -> (x._2 ++ List(
        (Noun, Done(ColumnOp(defaultOp.get, BaseColumn(tableName, columnName, false)))),
        (CompoundingNoun, argMatch[LFNode](1, { case (relation: RelationLike) :: Nil => ColumnOp(defaultOp.get, ColumnOf(relation, columnName)) }))
      ))
    }
    x
  }

  protected def column(tableName: String, columnName: String, names: Seq[String]) = {
    names -> List(
      (Noun, Done(BaseColumn(tableName, columnName, false))),
      // Handle "qualified" column references like "opportunity name" => BaseColumn(Opportunity, Name)
      // This flag allows the LF -> logical SQL generator to search for the join for this column
      // This is basically the ONE place where we switch the flag
      (CompoundingNoun, argMatch[LFNode](1, { case (relation: BaseRelation) :: Nil if relation.tableName == tableName => BaseColumn(tableName, columnName, true) }))
    )
  }

  // TODO: filter is on a particular table
  protected def adjectiveFilter(tableName: String, filter: String): List[(CcgCat, SemanticState)] = {
    adjectiveFilter(tableName, new sqlkit.FilterPred(filter))
  }

  protected def adjectiveFilter(tableName: String, filter: sqlkit.FilterExpr): List[(CcgCat, SemanticState)] = {
    val predicate = BasePredicate(tableName, filter)
    List(
      (Adjective, argMatch[LFNode](1, { case (rel: RelationLike) :: Nil => Filter(rel, predicate) })),
      (Adjective, argMatch[LFNode](1, { case (column: ColumnLike) :: Nil => column match {
        // These are to support scalar expressions like "total wellness deal revenue last month"

        // Previously, in the expression "deal revenue per month last year in sf" "month last year in sf" would
        // become a column over a filtered relation.  Instead, we want the "last year in sf" to attach to the grouped
        // aggregate expression.  Commenting out these two lines accomplishes this, while still covering the scalar
        // case "total deal revenue last month"

        //case ColumnOf(rel, columnName) => ColumnOf(Filter(rel, predicate), columnName)
        case ColumnOp(op, BaseColumn(tableName, columnName, _)) => ColumnOp(op, ColumnOf(Filter(BaseRelation(tableName), predicate), columnName))
        case ColumnOp(op, DummyColumn(rel)) => ColumnOp(op, DummyColumn(Filter(rel, predicate)))
        //case BaseColumn(tableName, columnName) => ColumnOf(Filter(BaseRelation(tableName), predicate), columnName)
        case ColumnOp(op, ColumnOf(rel, columnName)) => ColumnOp(op, ColumnOf(Filter(rel, predicate), columnName))
      }}))
    )
  }

  /**
   * This is for handling "biotechnology industry accounts".
   * @return A Filter over accounts with the predicate industry=biotechnology
   */
  protected def adjectiveColumnFilter(tableName: String, column: String, values: Seq[String]) = {
    List[(CcgCat, SemanticState)](
      (ForwardCat(Adjective, Noun), argMatch[LFNode](2, {
        case BaseColumn(argTableName, argColumnName, _) :: (rel: RelationLike) :: Nil
          if argTableName == tableName && argColumnName == column => Filter(rel, BasePredicate(tableName, BinaryPredicate("t." + column, "=", List(LiteralValue(values.head)))))
      }))
    )
  }

  protected def timeFrame(v: DateLikeValue) = {
    /**
     * The $prominentDateColumn thing is only used in the old SQL demos
     * We have no plans to resurrect it in the Salesforce case
     * And it for whatever reason causes an explosion of parses (partly because adjectiveFilters() can attach
     * to almost anything
     * So let's suppress it
     */
    //(adjectiveFilter("_", BinaryPredicate("t.$prominentDateColumn", "=", List(v))) ++:
      List((Noun, Done(Literal(v))))
  }
}

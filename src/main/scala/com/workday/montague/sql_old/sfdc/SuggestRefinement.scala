package sfdc

import metadata.Metadata
import natural._
import scala.Some
import parser.NonTerminal

/**
 * Given a query, generate refinements of the query to suggest to the user.
 */
class SuggestRefinement(md: Metadata) {
  /**
   * Map from SFDC table name to refinements of a query that should produce a chart.
   * A refinement is given as a function from String => String, taking an input query and returning a new
   * query.
   */
  private[this] val chartRefinements: Map[String, List[String => String]] = Map(
    "Opportunity" -> List(
      "total amount from " + _ + " with users per user full name",
      "total amount from " + _ + " with accounts per account name",
      "total amount from " + _ + " per stage",
      "total amount from " + _ + " with accounts per account industry",
      "total amount from " + _ + " per won",
      "total amount from " + _ + " with users per user department",
      "total amount from " + _ + " with accounts per account type",
      "total amount from " + _ + " with accounts per account source"
    ),
    "Account" -> List(
      "total amount from opportunities related to " + _ + " per stage",
      "number of " + _ + " per industry"
    )
  )

  /**
   * Generate refinements of a row-level (i.e. non-aggregate) query that will produce a chart (i.e. we add
   * an aggregate and a group by to the query)
   *
   * @param query the query to refine
   * @param lf the logical form for the query.  This is needed to figure out what table the input query is against.
   * @return A list of refined queries in English that should produce a chart where the head query we believe is
   *         the "best" one or
   *         Nil if a refined query could not be generated.
   */
  def generateAllChartRefinements(query: String, lf: RelationLike): List[String] = {
    SqlGen.findUnderlyingTableName(lf).toList flatMap { tableName =>
      chartRefinements.get(tableName).toList flatMap { refiners =>
        refiners map { refiner =>
          refiner.apply(query)
        }
      }
    }
  }

  /**
   * Generate queries for drilling down on a chart.  For example, if your chart is for the query
   *
   * "total open opportunity amount per stage"
   *
   * Then we generate queries like "open opportunity where stage is Prospecting" etc.
   *
   * @param parsesql the parse for the chart, which should be an aggregate query
   * @param values the values that the chart takes on
   */
  def generateChartDrill(parsesql: ParseSql, values: Iterable[String]): Map[String, String] = {
    val parse = parsesql.parse
    val baseQueryStrOpt = findBaseQueryStrs(parse, md)
    baseQueryStrOpt map { baseQueryStr =>
      val relationStr = baseQueryStr._1
      val groupingStr = baseQueryStr._2
      values.map(value => (value, s"$relationStr where $groupingStr is $value")).toMap
    } getOrElse(Map())
  }

  /**
   * Given a parse, suppose the parse represents an aggregate query.  The find the sub-phrases of the
   * parse that represent the relation underlying the aggregate query, and the grouping.
   *
   * For example if we have "total open opportunity amount per stage"
   *
   * Then the underlying relation is "open opportunity" and the underlying grouping is "stage"
   *
   * @param parse the parse of a query that should be an aggregate query
   * @return Some((relationQuery, groupingStr)) the English queries for the underlying relation and grouping of the
   *         aggregate query or
   *         None if the query was not a suitable aggregate query, or the underlying sub-phrases could not be
   *         determined
   */
  private[this] def findBaseQueryStrs(parse: NQParseNode, md: Metadata): Option[(String, String)] = {
    val baseQueryStrOpt = parse.flatMapLogicalForm { lf: LFNode =>
      // Take the top-most logical form and extract the LF's for the relation and the grouping
      val relationAndGroupingOpt = destructureAggregateLogicalForm(lf)
      relationAndGroupingOpt flatMap { relationAndGrouping =>
        val relationLf = relationAndGrouping._1
        val groupingLf = relationAndGrouping._2
        // Now search for the LF's for the relation and the grouping in the sub-trees of this parse
        parse.findLogicalForm(groupingLf) flatMap { groupingNode =>
          relationLf match {
            case BaseRelation(tableName) =>
              md.getTable(tableName) map { table =>
                (table.displayLabel, groupingNode.parseTokenString)
              }
              Some((tableName, groupingNode.parseTokenString))
            case _ => parse.findLogicalForm(relationLf) map { relationNode =>
              // Take the English strings from those sub-trees
              (relationNode.parseTokenString, groupingNode.parseTokenString)
            }
          }
        }
      }
    }
    baseQueryStrOpt
  }

  /**
   * Given a logical form lf, suppose lf represents an aggregate query.  Then find the underlying relatino
   * and grouping of the query.
   *
   * For example for "total open opportunity amount per stage" we should have a LF similar to
   *    AggregateBy(ColumnOp("sum", Filter(BaseRelation("Opportunity"), "status = open")), BaseColumn("Opportunity", "stage"))
   *
   * From that we want to extract relationLf = Filter(BaseRelation...) and groupingLf = BaseColumn("Opportunity", "stage")
   *
   * @param lf a logical form that should represent an aggregate query
   * @return Some((relationLf, groupingLf)) the logical forms for the underlying relation and grouping or
   *         None if the LF was not a suitable aggregate query
   */
  private[this] def destructureAggregateLogicalForm(lf: LFNode): Option[(LFNode, LFNode)] = {
    // TOOD: handle transposed relations
    val relationAndGroupingOpt = lf match {
      case AggregateBy(ColumnOp(op, BaseColumn(tableName, columnName, _)), Some(grouping)) =>
        Some((BaseRelation(tableName), grouping))
      case AggregateBy(ColumnOp(op, ColumnOf(relation, columnName)), Some(grouping)) =>
        Some((relation, grouping))
      case AggregateBy(ColumnOp("count", DummyColumn(relation)), Some(grouping)) =>
        Some((relation, grouping))
      case _ => None
    }
    relationAndGroupingOpt
  }

  /**
   * Return a list of AlterableNode instances that break down the query into alterable or non-alterable constituents
   *
   * For example, if we have:
   * "open opportunities where stage is closed won with accounts in the biotechnology industry where billing state/province is CA"
   *
   * Then we return a List with:
   * AlterableNode("open", true)
   * AlterableNode("opportunities", false)
   * AlterableNode("where stage is closed won", true)
   * AlterableNode("with accounts", false)
   * AlterableNode("in the biotechnology industry", true)
   * AlterableNode("where billing state/province is CA", true)
   *
   * @param parsesql
   * @return
   */
  def generateAlterableRefinements(parsesql: ParseSql) = {
    import scala.collection.mutable

    val parse = parsesql.parse
    var alterableNodes = parse.allNodes.flatMap(findAlterableNode(_))
    alterableNodes = alterableNodes.sortBy(_.spans.start)

    // Merge the terminals and alterable nodes.  Only merge in the terminals that aren't already contained in the alterables
    val result = new mutable.ListBuffer[AlterableNode]
    val terminals = parse.terminals.toIndexedSeq
    var iTerminal = 0
    var iAlterable = 0
    while(iTerminal < terminals.size && iAlterable < alterableNodes.size) {
      val terminal = terminals(iTerminal)
      val alterable = alterableNodes(iAlterable)
      if (terminal.spans.isContainedIn(alterable.spans)) {
        // Skip this terminal because it's contained in one of the breakdowns
        iTerminal += 1
      } else if (terminal.spans < alterable.spans) {
        result += AlterableNode(terminal.parseTokenString, false)
        iTerminal += 1
      } else if (alterable.spans < terminal.spans) {
        result += AlterableNode(alterable.parseTokenString, true)
        iAlterable += 1
      } else {
        throw new IllegalStateException("merge of breakdown didn't work")
      }
    }
    // Add any remaining tails
    result ++= alterableNodes.drop(iAlterable).map(n => AlterableNode(n.parseTokenString, true))
    result ++= terminals.drop(iTerminal).map(n => AlterableNode(n.parseTokenString, false))
    result.toList
  }

  /**
   * Given a node representing a sub-tree/span of the parse, determine whether the span is alterable and if so,
   * which part (sub-span) of the span can be altered in what way
   *
   * Right now we take the parse node, we look at its semantic form and we only look for Filters.
   *
   * e.g.
   *
   *    open opportunity with name is Big Deal
   *
   * Then we have a semantic form like:
   *
   * Filter(Filter(opportunity, status = open), name = Big Deal)
   *
   * In that case we go back to the parse node and take the child "operatorNode".  The argumentNode will always
   * be the relation that's being filtered.  The operatorNode will always be the segment of parse that is modifying
   * the relation.
   *
   * @return Some(parsenode) pointing to a sub-node of the given node that can be removed from the parse
   *         None if the given node can't be altered in any way that we know
   */
  private[this] def findAlterableNode(node: NQParseNode) = {
    node match {
      case nonterminal: NonTerminal[CcgCat] =>
        val lfOpt = node.logicalFormOption[LFNode]
        lfOpt flatMap { lf =>
          lf match {
            case Filter(rel, pred) =>
              Some(nonterminal.operatorNode)
            case _ => None
          }
        }
      case _ => None
    }
  }
}

/**
 * Information about the alterability of a span
 * @param parsertoken the tokens of the span
 * @param removable whether the span is removable or not
 */
case class AlterableNode(parsertoken: String, removable: Boolean)
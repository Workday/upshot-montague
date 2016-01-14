
/**
 * See also documentation in docs/parser.txt
 */

package natural

import sqlkit.SortDirection

/** Natural query logical form */

// We can have two approaches here:
// all constructors could take LFNode as the only argument type and type checking happens later
// OR, we could enforce type constraints here
// TODO: this should take as a type parameter a metadata scheme
// One thing we could do to get around the argument ordering is to map the arguments by type
// So for a given predicate we specify the number of arguments and their expected types, and we map
// the actual arguments by looking at their types
sealed trait LFNode
trait RelationLike extends LFNode {
  def isBaseRelationLike: Boolean
}
trait ColumnLike extends LFNode // we may want to treat this as a 1-column relation
trait Predicate extends LFNode
// TODO: trait ValueLike, NumericValue, etc.
case class DummyColumn(rel: RelationLike) extends ColumnLike // basically for count(1), although interestingly this in effect coerces a relation to a column
// Reference to a table alone.
case class BaseRelation(tableName: String) extends RelationLike {
  override def isBaseRelationLike = true
}
sealed trait JoinType
case object InnerJoin extends JoinType
case object AntiJoin extends JoinType
case object OuterJoin extends JoinType
case class Relate(rel1: RelationLike, rel2: RelationLike, joinType: JoinType) extends RelationLike {
  override def isBaseRelationLike = true // TODO: this needs work
}
case class ColumnOf(relation: RelationLike, columnName: String) extends ColumnLike

/**
 * @todo Ideally, we wouldn't have isQualified here - we'd have it in some kind of feature data structure
 *       that sits alongside the semantic state
 * @param isQualified Indicates whether the reference to this column specified the table.  Always false
 *                    unless the compound noun rule in DictHelpers.column() applies.
 *                    E.g. "opportunities by industry" - "industry" is unqualified
 *                    "opportunities by account industry" - "account industry" is qualified
 */
case class BaseColumn(tableName: String, columnName: String, isQualified: Boolean) extends ColumnLike // unclear if this is the right way to go
case class ColumnOp(op: String, column: ColumnLike) extends ColumnLike
//case class RelationOp(op: String, relation: RelationLike) extends ColumnLike // basically for count, # of, although we could maybe use this for "largest" and "smallest" or we could get rid of this
case class Filter(relation: RelationLike, predicate: Predicate) extends RelationLike {
  override def isBaseRelationLike = relation.isBaseRelationLike
}
case class OrganizeBy(relation: RelationLike, column: ColumnLike, sortDirection: SortDirection) extends RelationLike {
  override def isBaseRelationLike = false
}
// TODO: note that an aggregate with a grouping is a relation, but without a grouping it is a value
case class AggregateBy(agg: ColumnOp, grouping: Option[ColumnLike]) extends RelationLike { // aggregateby is just organizeby with a columnOp
  override def isBaseRelationLike = false
}
case class BasePredicate(tableName: String, pred: sqlkit.FilterExpr) extends Predicate // predicates need to identify what table they operate against
object BasePredicate {
  def apply(tableName: String, pred: String) = new BasePredicate(tableName, new sqlkit.FilterPred(pred))
}
/**
 * A predicate that is dynamically generated at LF => logical SQL translation time (nqlfsql.scala).
 * @param generatePredicateFn A function that generates the predicate.  The function takes as input the relation
 *                            against which the predicate is being applied e.g. BaseRelation("Account").  The
 *                            output of the function is a "concrete predicate" i.e. an instance of BasePredicate
 *                            that can be applied to the relation.
 */
case class DynamicPredicate(generatePredicateFn: RelationLike => Option[BasePredicate]) extends Predicate
//case class BasePredicate(filter: String) extends Predicate // predicates need to identify what table they operate against
case class Limit(n: Int, relation: RelationLike) extends RelationLike { // coalesce with below and change sort to Option[sort]
  override def isBaseRelationLike = true
}
case class LimitBy(n: Int, relation: RelationLike, sort: ColumnLike, sortDir: SortDirection) extends RelationLike {
  override def isBaseRelationLike = true // ??
}
case class Literal[T](value: T) extends LFNode
// what about trace/unary transformations i.e. I see a patterned sequence of nodes so I turn that into a higher-level thing
// also TODO: redundant things - these have a similar implementation to the previous


// Note this extends sqlkit.Value, which clients may use when declaring literal BinaryPredicates
case class RelationValue(rel: RelationLike) extends sqlkit.Value

trait NQMetadata {
  def getTableInfo(tableName: String): Option[NQTableInfo]
  def getJoinInfo(tableName1: String, tableName2: String): Option[String]
  def getJoinInfo(fromAndTo: (String, String)): Option[String] = getJoinInfo(fromAndTo._1, fromAndTo._2)
}

trait NQTableInfo {
  def fromExpr: String
  def prominentNumericColumnOpt: Option[String] = None
  def prominentDateColumn: Option[String] = None

  /** @return Nil by default, meaning display _all_ columns */
  def getDisplayColumns: List[String] = Nil

  def getColumn(columnName: String): Option[NQColumnInfo] = None

  /** The column expression to use to generate a count(*) */
  def countProj: String = "*"
}

import sqlkit.BinaryPredicate

trait NQColumnInfo {
  def generatePredicate(pred: BinaryPredicate): Option[BinaryPredicate]
}


// "largest deals by state"
// "largest deals by amount"

// analytic functions dont really nest:  largest 5 deals per state is that OrganizeBy(LimitBy)?  the sql is more like Filter(Extend(rank))
// maybe we should keep OrganizeBy, add a limit N to the organizeBy so it's more like a for-each statement, and also add a RankBy function?
// a ForEach statemnet is really what we want and generalizes to nested groupby's, sortby's, some analytic functions (windowed?)
// ForEachGroup
// ByGroup
// vs regular sql GroupBy(Limit) applies the limit then the group by.  But ForEachGroup takes the child query and runs it for each group
// almost makes it like an operation with separate relation and operation operands i.e. ForEachGroup(relation, grouping, operation)
// where operation is a function from relation -> relation
// "largest deal per state"

// should we be refining the ColumnLike type into Categorial, Numeric, etc.?

// queries to show the dictionary directly

// coercions:  these are always added by the post-processor
// we may even encode these differently - not as LFNode's but as a function or a typeclass (defining a method getSelectBuilder)?
//case class AsRelation(column: ColumnLike) extends RelationLike

// To support # of deals we need to extend with the following machinery:
// RelationOp
// Lift to Relation (to support # of deals)
// AggregateBy(columnLike, grouping)
// alternatively:  we could re-use ColumnOp("count", dummyColumn) and just add DummyColumn(relation)
// we still need lift to relation to support "# of deals" or any other aggregate anyway (total deal value)

// largest deals per quarter
// largest deal per quarter
// largest deals per quarter by amount

// TODO: case class RelatedTo(relation: RelationLike, to: RelationLike, via: Option[RelationLike, ColumnLike]) // filter, join in, and add columns from a related object
// these LFNodes are representations of linguistic idioms


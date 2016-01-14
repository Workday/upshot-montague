/**
 * Methods and objects from NaturalQuery that are more related to generating ParseTokens from the input.
 */

package natural

import parser._
import parser.semantics._
import sqlkit._
import parser.ParseToken
import sqlkit.LiteralValue
import scala.Some
import sqlkit.BinaryPredicate
import sqlkit.RelativeDateRangeValue
import parser.Spans
import org.apache.commons.lang.StringEscapeUtils.escapeJava
import _root_.util.Logging

sealed trait DictSource
case object MainDict extends DictSource     // Tokens came from the main dictionary
case object Fallback extends DictSource     // Tokens came from a "fallback" dictionary
/// @todo Also include TokenMatchers?
/// (It seems dangerous to include things as coming from MainDict that don't come from the mainDict flatmap.
/// That's prone to error, and not including the terms here.)
case object StarDict extends DictSource     // Tokens that came from the star dict

/**
 * NaturalQueryParsers contain their own terminal type NQTerminal, and don't use parser.Terminal.
 * @todo It's pretty inefficient to store the ParseToken here, because it gets duplicated so much. Instead,
 *       the chart (or something) should store all the ParseTokens, and the Terminals should just store the Span within
 *       the chart.
 */
case class NQTerminal(cat: CcgCat, sem: SemanticState, parseToken: ParseToken, spans: Spans, source: DictSource) extends SemanticParseNode[CcgCat](cat, sem) {
  override def toString: String = toStringHelp(withSemantics=true)
  //  override def toStringIndented(linePrefix: String): String = toString
  def toStringHelp(indent: String = "", currentIndent: String = "", withSemantics: Boolean = false): String = {
    val str = s"('${cat.toString}', $parseToken, ${source.toString})"
    if (withSemantics) s"${sem.toString}, $str"
    else str
  }
  def toDotStringHelp(pre: String): String = {
    pre + " [shape=none,style=filled,fillcolor=lightblue,margin=0,pad=0,label=\"" + escapeJava(cat.toString + "\n" + sem.toString + "\n" + parseTokenString) + "\"];\n" +
    "{rank = max; \"tokens\"; " + pre + "}\n"
  }
  def parseTokens = List(parseToken)
  def terminals = List(this)
  def parseTokenString = parseToken.tokenString

  def children = Nil

  def isFallback: Boolean = (source == Fallback)
    ///     This is the WRONG way to find Literals
    //      sem match {
    //        case Done(Literal(_)) => {

  def isLiteral: Boolean = {
    sem match {
      case Done(Literal(RelativeDateRangeValue(_, _))) => false    // This sucks
      case Done(Literal(_)) => true
      case _ => {
        if (isFallback) {
          logger.warn("isFallback tokens should be Done(Literal): " + toString)
          true
        } else { false }
      }
    }
  }
}

/// @todo Since this is basically about generating ParseTokens, rename this.
/**
 *
 * This is our core parser.
 * It extends the SemanticParser, and fixes the syntactic scheme to be CCG.
 * It takes as input a dictionary, and augments it with another dictionary of
 * pre-defined tokens.
 *
 * Semantic sensicality constraints e.g. "sum of industry" is non-sensical
 * are applied after parsing.
 *
 * This allows us to form the semantic tree using more complex logic
 * e.g. "top deals by industry" we can infer that industry the column off of deal
 * since deal is in scope
 *
 * However this means semantic information is not in play when forming
 * probabilities during parsing
 *
 * In that case, especially since we're doing so much forward and backward
 * applications, we are really just creating trees of token jumbles
 *
 * @param dict map from a set of synonyms -> list of (ccg category, IterSemantic) possible syntactic + semantic interpretations
 */
class NaturalQueryParser(dict: NQDict, starDictOverride: Option[NQDict] = None,
                         override protected val timeLimitSecs: Double) extends SemanticParser[CcgCat] with DictHelpers with Logging {
  private[this] val flattenedDict = makeFlattenedDict(dict)
  private[this] lazy val flattenedCoreDict = makeFlattenedDict(coreDict)

  // @todo Exclude synonyms from the starDict?
  // @todo Include TokenMatcher stuff here?
  private[this] lazy val starDict = starDictOverride.map(makeFlattenedDict(_)).getOrElse(flattenedDict)
  // @todo Add coreDict here? Except then things break and I don't know why.
//  private[this] lazy val starDict = makeFlattenedDict(coreDict ++ dict)

  logger.debug("NaturalQueryParser initialized with dict size = " + dict.size + ", dict values size = " + dict.values.map(_.size).sum + ", flattened dict size = " + flattenedDict.keys.size + ", entries size = " + flattenedDict.values.map(_.size).sum + ", star dict entries size = " + starDict.values.map(_.size).sum)

  /**
   * Take a sequence of tokens as a phrase e.g. "Account Team".  Look up in the dictionary to see if there are
   * meanings defined for that phrase in the dictionary.
   */
  // TODO: treat parentheticals by making two copies - unparenthesized, and elided
  override protected def dictLookup(parseToken: ParseToken, spans: Spans): List[Node] = {
    import scala.collection.mutable
    val parseTokenStr = parseToken.tokenString
    var resultEntries = new mutable.ListBuffer[NQParseNode]
    resultEntries ++= (for(entry <- flattenedDict.getOrElse(parseTokenStr, Nil))
    yield NQTerminal(entry._1, entry._2, parseToken, spans, MainDict))

    resultEntries ++= (for(entry <- flattenedCoreDict.getOrElse(parseTokenStr, Nil))
    yield NQTerminal(entry._1, entry._2, parseToken, spans, MainDict))

    // @todo Rewrite so that TokenMatchers is applied identically, i.e. it's an NQDict to flattenedMap
    resultEntries ++= TokenMatchers.matchers.flatMap(_.apply(parseTokenStr).map(entry => NQTerminal(entry._1, entry._2, parseToken, spans, MainDict)))

    //    val starEntries = for(entry <- starDict.getOrElse(parseTokenStr, Nil))
    //      yield NQTerminal(entry._1, entry._2, parseTokenStr, MainDict)
    //    println(parseTokenStr)
    if (Array("*TRACE*", "*SUB1*", "*SUB2*") contains parseTokenStr) {
      //      println("FOUND IT")
      for (fullentry <- starDict) {
        // Unfortunately, we lose the parseTokenization of fullentry._1, and pretend it's a single token.
        val explicitStarParseTokenTokens = Seq(parseTokenStr, fullentry._1)
        val starEntries = for (entry <- fullentry._2)
        yield NQTerminal(entry._1, entry._2, ParseToken(tokens = explicitStarParseTokenTokens), spans, StarDict)
        resultEntries ++= starEntries
      }
      // NOTE: the chart parser later deduplicates these in SemanticParser.postStep()
    }
    resultEntries.toList
  }

  override protected def fallbackEntry(parseToken: ParseToken, spans: Spans): List[NQParseNode] = {
    if (parseToken.tokens.length <= 2)
      makeLiteral(parseToken).map(literal => NQTerminal(Noun, Done(literal), parseToken, spans, Fallback)).toList
    else
    /*
     We currently ignore literals of length > 2, because they are typically bogus in our examples.
    once we have a better parser, and recognize more common words, then we should add back in long literals

    Another nice thing we can do is, when we do a "did you mean" the involves literals, we can render it using
    quotes around the literal. This will make the "did you mean" look more reasonable, and also train the user to
    use quotes to mean literal. We can then penalize literal queries that don't involve quotes.
     */
      Nil
  }


  private[this] def makeLiteral(parseToken: ParseToken) = {
    var result: Option[Literal[_]] = makeLiteralInt(parseToken)
    if (result.isEmpty) {
      result = Some(Literal[String](parseToken.tokenString))
    }
    result
  }

  private[this] def makeLiteralInt(parseToken: ParseToken) = {
    if (parseToken.tokens.size == 1) {
      try {
        val bd = BigDecimal.apply(parseToken.tokens.head.filter(c => c != '$' && c != ',')) // recognized $100,000 as a number
        Some(Literal[BigDecimal](bd))
      } catch {
        case _: NumberFormatException => None
      }
    } else {
      None
    }
  }

  /**
   * This is for "total".  This prevents "total deal revenue per month" from turning into a count(*) while still
   * allowing "total deals per month" to become a count(*)
   */
  private[this] def isBaseRelationLike(rel: RelationLike): Boolean = {
    rel.isBaseRelationLike
  }

  import scala.collection.mutable
  import scala.collection.immutable.ListMap

  /**
   * Generate a "flattened" dictionary from an NQDict.  The NQDict is a mapping from a set of synonyms to meanings.
   * The flattened dict maps each token to its meanings.
   */
  private[this] def makeFlattenedDict[E](keymap: Map[Seq[String], List[E]]): Map[String, List[E]] = {
    /**
     * Convert a map from Seq[A] -> List[B], to A -> List[B], flattening out the A's and concatenating any
     * duplicate entries of an A
     */
    def multikeymap[A, B](keymap: Map[Seq[A], List[B]]): Map[A, List[B]] = {
      val map = mutable.LinkedHashMap[A, List[B]]()
      for {
        (keys, values) <- keymap
        key <- keys
      } {
        if (map.contains(key)) {
          map(key) ++= values
        } else {
          map(key) = values
        }
      }
      map.toMap
    }

    // toLowerCase must come first so that we can concatenate duplicates properly
    // @todo: We shouldn't do toLowerCase here
    val loweredKeymap: Map[Seq[String], List[E]] = mutable.LinkedHashMap(keymap.toList.map(x => (x._1.map(_.toLowerCase), x._2)): _*).toMap
    multikeymap(loweredKeymap)
  }

  // map from a set of synonyms -> list of (ccg category, IterSemantic) possible syntactic + semantic interpretations
  val coreDict: NQDict = Map(
      Seq("sum") -> List(
        // "sum of amount"
        (CompoundingNoun, argMatch[LFNode](1, { case (column: ColumnLike) :: Nil => ColumnOp("sum", column) }))
      ),
      Seq("total") -> List(
        (Adjective, argMatch[LFNode](1, { case (column: ColumnLike) :: Nil => ColumnOp("sum", column) })),
        (Adjective, argMatch[LFNode](1, { case (rel: RelationLike) :: Nil if isBaseRelationLike(rel) => ColumnOp("count", DummyColumn(rel)) } ))
      ),
      Seq("average", "avg") -> List(
        (Adjective, argMatch[LFNode](1, { case (column: ColumnLike) :: Nil => ColumnOp("avg", column) }))
      ),
      Seq("top", "best", "largest", "biggest") -> List(
        (Adjective, argMatch[LFNode](1, { case (relation: RelationLike) :: Nil => OrganizeBy(relation, BaseColumn("_", "$prominentNumericColumn", false), Desc) })),
        (ForwardCat(ForwardCat(Noun, Noun), Adjective), argMatch[LFNode](2, { case (lit@Literal(n: Int)) :: (org: OrganizeBy) :: Nil => LimitBy(n, org.relation, org.column, Desc) })),
        (ForwardCat(ForwardCat(Noun, Noun), Adjective), argMatch[LFNode](2, { case (lit@Literal(n: Int)) :: (rel: RelationLike) :: Nil => LimitBy(n, rel, BaseColumn("_", "$prominentNumericColumn", false), Desc) }))
      ),
      Seq("bottom", "worst", "smallest", "littlest", "tiniest") -> List(
        (Adjective, argMatch[LFNode](1, { case (relation: RelationLike) :: Nil => OrganizeBy(relation, BaseColumn("_", "$prominentNumericColumn", false), Asc) })),
        (ForwardCat(ForwardCat(Noun, Noun), Adjective), argMatch[LFNode](2, { case (lit@Literal(n: Int)) :: (org: OrganizeBy) :: Nil => LimitBy(n, org.relation, org.column, Asc) })),
        (ForwardCat(ForwardCat(Noun, Noun), Adjective), argMatch[LFNode](2, { case (lit@Literal(n: Int)) :: (rel: RelationLike) :: Nil => LimitBy(n, rel, BaseColumn("_", "$prominentNumericColumn", false), Asc) }))
      ),
      // "5 accounts" means show me any 5 accounts and "top 5 accounts" means show me 5 accounts sorted by X
      /*
    Seq(Seq(NumericConstant, TextNumericConstant) ->
      //List(LimitOp, Constant)
      ), */
      Seq("by") -> List(
        // "# deals by stage" - the aggregateby should have precedence
        // Assume that the user means they want something descending ...
        (Preposition, argMatch[LFNode](2, { case (column: ColumnLike) :: (rel: RelationLike) :: Nil => OrganizeBy(rel, column, Desc) })), // not sure if "per" should go to this case
        (Preposition, argMatch[LFNode](2, { case (grouping: ColumnLike) :: (agg: ColumnOp) :: Nil => AggregateBy(agg, Some(grouping)) }))
      ),
      Seq("per", "broken down by", "break down by", "grouped by", "grouping by", "grouped according to", "broken down according to") -> List(
        (Preposition, argMatch[LFNode](2, { case (column: ColumnLike) :: (AggregateBy(x, None)) :: Nil => AggregateBy(x, Some(column))
        })),
        (Preposition, argMatch[LFNode](2, { case (grouping: ColumnLike) :: (agg: ColumnOp) :: Nil => AggregateBy(agg, Some(grouping)) }))
      ),
      // @todo: This really belongs in a Salesforce-specific core dictionary
      /** "me" is modeled as a Filter over the User table with Id = my user id */
      Seq("me") ->
        List(
          (Noun, Done(Filter(BaseRelation("User"), BasePredicate("User", BinaryPredicate("t.Id", "=", List(SqlVariable("userId")))))))
        ),
      Seq("of") ->
        List(
          (OperatingPreposition, of)
        ),
      /**
       * How we treat filler words:
       *  - We don't actually ignore them. We just map them to the identity semantic, which is a (ccg category, lambda) pair
       *    where the ccg category takes the ccg cat of the argument X and returns the same category as its output. The lambda
       *    similarly is the identity lambda.
       *  - We have the usual CCG option of taking the argument on the right-hand side, the left-hand side or both. I actually
       *    make use of that so that these words, for example, only accept an argument on the right-hand side (forward slash).
       */
      Seq("find", "list", "locate", "query") ->
        List(
          (ForwardIdentityCat, identity)
        ),
      Seq("the", "in", "that are", "all") ->
        List(
          (ForwardIdentityCat, identity)
        ),
      Seq("is", "equals", "is equal to", "equal to", "=", "==") ->
        // TODO: "opportunities amount greater than 3000"
        List(
          // @todo the way dates are currently being treated as special values seems unnecessary
          (TransitiveVerb, argMatch[LFNode](2, { case (literal: Literal[_]) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
            BasePredicate(tableName, BinaryPredicate("t." + columnName, "=", List(LiteralValue(literal.value)))) })),
          (TransitiveVerb, argMatch[LFNode](2, { case (Literal(value: DateLikeValue)) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
            BasePredicate(tableName, BinaryPredicate("t." + columnName, "=", List(value))) })),
          /** "accounts where owner is me", here "me" is a relation acting as a value */
          (TransitiveVerb, argMatch[LFNode](2, { case (rel: RelationLike) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
            BasePredicate(tableName, BinaryPredicate("t." + columnName, "=", List(RelationValue(rel)))) }))
        ),
    Seq("is not", "does not equal", "is not equal to", "not equal to", "!=", "<>") ->
      List(
        // @todo the way dates are currently being treated as special values seems unnecessary
        (TransitiveVerb, argMatch[LFNode](2, { case (literal: Literal[_]) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
          BasePredicate(tableName, BinaryPredicate("t." + columnName, "!=", List(LiteralValue(literal.value)))) })),
        (TransitiveVerb, argMatch[LFNode](2, { case (Literal(value: DateLikeValue)) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
          BasePredicate(tableName, BinaryPredicate("t." + columnName, "!=", List(value))) })),
        /** "accounts where owner is me", here "me" is a relation acting as a value */
        (TransitiveVerb, argMatch[LFNode](2, { case (rel: RelationLike) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
          BasePredicate(tableName, BinaryPredicate("t." + columnName, "!=", List(RelationValue(rel)))) }))
      ),
      Seq("contains") ->
        List(
          (TransitiveVerb, argMatch[LFNode](2, { case (literal: Literal[_]) :: (column@BaseColumn(tableName, columnName, _)) :: Nil => BasePredicate(tableName, BinaryPredicate("t." + columnName, "contains", List(LiteralValue(literal.value)))) }))
        ),
      Seq(">", "is greater than", "greater than", "more than", "over") ->
        List(
          (TransitiveVerb, argMatch[LFNode](2, { case (literal@Literal(value: Number)) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
            BasePredicate(tableName, BinaryPredicate("t." + columnName, ">", List(LiteralValue(value)))) }))
        ),
      Seq(">", "after", "is after", "is greater than", "greater than", "more than", "over") ->
        List(
          (TransitiveVerb, argMatch[LFNode](2, { case (literal@Literal(value: DateLikeValue)) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
            BasePredicate(tableName, BinaryPredicate("t." + columnName, ">", List(value))) }))
        ),
      Seq(">=", "is greater than or equal to", "greater than or equal to", "greater or equal to") ->
        List(
          (TransitiveVerb, argMatch[LFNode](2, { case (literal@Literal(value: Number)) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
            BasePredicate(tableName, BinaryPredicate("t." + columnName, ">=", List(LiteralValue(value)))) }))
        ),
      Seq(">=", "after or equal to", "is after or equal to", "is greater than or equal to", "greater than or equal to", "greater or equal to") ->
        List(
          (TransitiveVerb, argMatch[LFNode](2, { case (literal@Literal(value: DateLikeValue)) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
            BasePredicate(tableName, BinaryPredicate("t." + columnName, ">=", List(value))) }))
        ),
      Seq("<", "is less than", "less than", "fewer than", "under") ->
        List(
          (TransitiveVerb, argMatch[LFNode](2, { case (literal@Literal(value: Number)) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
            BasePredicate(tableName, BinaryPredicate("t." + columnName, "<", List(LiteralValue(value)))) }))
        ),
      Seq("<", "before", "is before", "is less than", "less than", "fewer than", "under") ->
        List(
          (TransitiveVerb, argMatch[LFNode](2, { case (literal@Literal(value: DateLikeValue)) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
            BasePredicate(tableName, BinaryPredicate("t." + columnName, "<", List(value))) }))
        ),
      Seq("<=", "is less than or equal to", "less than or equal to", "fewer than or equal to") ->
        List(
          (TransitiveVerb, argMatch[LFNode](2, { case (literal@Literal(value: Number)) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
            BasePredicate(tableName, BinaryPredicate("t." + columnName, "<=", List(LiteralValue(value)))) }))
        ),
      Seq("<=", "before or equal to", "is before or equal to", "is less than or equal to", "less than or equal to", "fewer than or equal to") ->
        List(
          (TransitiveVerb, argMatch[LFNode](2, { case (literal@Literal(value: DateLikeValue)) :: (column@BaseColumn(tableName, columnName, _)) :: Nil =>
            BasePredicate(tableName, BinaryPredicate("t." + columnName, "<=", List(value))) }))
        ),
      Seq("where", "with", "having") ->
        List(
          (ForwardCat(BackwardsCat(Noun, Noun), Sentence), argMatch[LFNode](2, { case (pred: Predicate) :: (rel: RelationLike) :: Nil => Filter(rel, pred) })),
          (ForwardCat(BackwardsCat(Noun, Noun), Noun), argMatch[LFNode](2, { case (pred: Predicate) :: (rel: RelationLike) :: Nil => Filter(rel, pred) }))
        ),
      Seq("with", "of", "from", "related to", "parented to", "that are children of", "belonging to") ->
        // "opportunities of/from/related to biotechnology accounts"
        List(
          (Preposition, argMatch[LFNode](2, { case List(table2: RelationLike, table1: RelationLike) if table2.isBaseRelationLike && table1.isBaseRelationLike => Relate(table1, table2, InnerJoin) }))
        ),
      Seq("without") ->
        List(
          (Preposition, argMatch[LFNode](2, { case List(table2: RelationLike, table1: RelationLike) if table2.isBaseRelationLike && table1.isBaseRelationLike => Relate(table1, table2, AntiJoin) }))
        ),
      Seq("with and without") ->
        List(
          (Preposition, argMatch[LFNode](2, { case List(table2: RelationLike, table1: RelationLike) if table2.isBaseRelationLike && table1.isBaseRelationLike => Relate(table1, table2, OuterJoin) }))
        ),
      Seq("from", "across") ->
        List(
          (ForwardIdentityCat, identity),
          // Rudimentary generic connection operation
          (ForwardCat(BackwardsCat(Noun, Noun), Noun), argMatch[LFNode](2, { case (rel: RelationLike) :: (column: ColumnLike) :: Nil if isBaseRelationLike(rel) =>
            column match {
              case agg@ColumnOp(op, BaseColumn(tableName, columnName, _)) => AggregateBy(ColumnOp(op, ColumnOf(rel, columnName)), None)
            }
          }))
        ),
      Seq("are there") ->
        List((BackwardIdentityCat, identity)),
      // These entries have to be changed to use prominentDateColumn, among other things.
      // They also need lexical entries that don't rely on prominentDateColumn, similar to the other superlative words in the dict.
      /*
        Seq("oldest", "earliest") ->
          List(
            (Adjective, argMatch[LFNode](1, { case (rel: RelationLike) :: Nil => OrganizeBy(rel, BaseColumn("deal", "closed_date"), Asc)})) // asc/desc
          ),
        Seq("newest", "latest") ->
          List(
            (Adjective, argMatch[LFNode](1, { case (rel: RelationLike) :: Nil => OrganizeBy(rel, BaseColumn("deal", "closed_date"), Desc)}))
          ),
          */
      Seq("number of", "total number of", "# of", "#", "count of", "count", "how many") ->
        List(
          // it's not really an adjective, but as far as CG effectively
          (Adjective, argMatch[LFNode](1, { case (rel: RelationLike) :: Nil if isBaseRelationLike(rel) => ColumnOp("count", DummyColumn(rel)) } ))
        ),
      // TODO: change all this stuff to _ or at least the nouns
      Seq("last year") -> timeFrame(RelativeDateRangeValue(Year, -1)),
      Seq("last 2 years") -> timeFrame(RelativeDateRangeValue(Year, -2)),
      Seq("last 3 years") -> timeFrame(RelativeDateRangeValue(Year, -3)),
      Seq("last 4 years") -> timeFrame(RelativeDateRangeValue(Year, -4)),
      Seq("last 5 years") -> timeFrame(RelativeDateRangeValue(Year, -5)),
      Seq("this year") -> timeFrame(RelativeDateRangeValue(Year,  0)),
      Seq("next year") -> timeFrame(RelativeDateRangeValue(Year, +1)),
      Seq("next 2 years") -> timeFrame(RelativeDateRangeValue(Year, +2)),
      Seq("next 3 years") -> timeFrame(RelativeDateRangeValue(Year, +3)),
      Seq("next 4 years") -> timeFrame(RelativeDateRangeValue(Year, +4)),
      Seq("next 5 years") -> timeFrame(RelativeDateRangeValue(Year, +5)),
      Seq("last fiscal year") -> timeFrame(RelativeDateRangeValue(Fiscal_Year, -1)),
      Seq("last 2 fiscal years") -> timeFrame(RelativeDateRangeValue(Fiscal_Year, -2)),
      Seq("last 3 fiscal years") -> timeFrame(RelativeDateRangeValue(Fiscal_Year, -3)),
      Seq("last 4 fiscal years") -> timeFrame(RelativeDateRangeValue(Fiscal_Year, -4)),
      Seq("last 5 fiscal years") -> timeFrame(RelativeDateRangeValue(Fiscal_Year, -5)),
      Seq("this fiscal year") -> timeFrame(RelativeDateRangeValue(Fiscal_Year,  0)),
      Seq("next fiscal year") -> timeFrame(RelativeDateRangeValue(Fiscal_Year, +1)),
      Seq("next 2 fiscal years") -> timeFrame(RelativeDateRangeValue(Fiscal_Year, +2)),
      Seq("next 3 fiscal years") -> timeFrame(RelativeDateRangeValue(Fiscal_Year, +3)),
      Seq("next 4 fiscal years") -> timeFrame(RelativeDateRangeValue(Fiscal_Year, +4)),
      Seq("next 5 fiscal years") -> timeFrame(RelativeDateRangeValue(Fiscal_Year, +5)),
      Seq("last month") -> timeFrame(RelativeDateRangeValue(Month, -1)),
      Seq("last 2 months") -> timeFrame(RelativeDateRangeValue(Month, -2)),
      Seq("last 3 months") -> timeFrame(RelativeDateRangeValue(Month, -3)),
      Seq("last 4 months") -> timeFrame(RelativeDateRangeValue(Month, -4)),
      Seq("last 5 months") -> timeFrame(RelativeDateRangeValue(Month, -5)),
      Seq("last 6 months") -> timeFrame(RelativeDateRangeValue(Month, -6)),
      Seq("last 7 months") -> timeFrame(RelativeDateRangeValue(Month, -7)),
      Seq("last 8 months") -> timeFrame(RelativeDateRangeValue(Month, -8)),
      Seq("last 9 months") -> timeFrame(RelativeDateRangeValue(Month, -9)),
      Seq("last 10 months") -> timeFrame(RelativeDateRangeValue(Month, -10)),
      Seq("last 11 months") -> timeFrame(RelativeDateRangeValue(Month, -11)),
      Seq("last 12 months") -> timeFrame(RelativeDateRangeValue(Month, -12)),
      Seq("this month") -> timeFrame(RelativeDateRangeValue(Month, 0)),
      Seq("next month") -> timeFrame(RelativeDateRangeValue(Month, +1)),
      Seq("next 2 months") -> timeFrame(RelativeDateRangeValue(Month, +2)),
      Seq("next 3 months") -> timeFrame(RelativeDateRangeValue(Month, +3)),
      Seq("next 4 months") -> timeFrame(RelativeDateRangeValue(Month, +4)),
      Seq("next 5 months") -> timeFrame(RelativeDateRangeValue(Month, +5)),
      Seq("next 6 months") -> timeFrame(RelativeDateRangeValue(Month, +6)),
      Seq("next 7 months") -> timeFrame(RelativeDateRangeValue(Month, +7)),
      Seq("next 8 months") -> timeFrame(RelativeDateRangeValue(Month, +8)),
      Seq("next 9 months") -> timeFrame(RelativeDateRangeValue(Month, +9)),
      Seq("next 10 months") -> timeFrame(RelativeDateRangeValue(Month, +10)),
      Seq("next 11 months") -> timeFrame(RelativeDateRangeValue(Month, +11)),
      Seq("next 12 months") -> timeFrame(RelativeDateRangeValue(Month, +12)),
      Seq("last quarter") -> timeFrame(RelativeDateRangeValue(Quarter, -1)),
      Seq("last 2 quarters") -> timeFrame(RelativeDateRangeValue(Quarter, -2)),
      Seq("last 3 quarters") -> timeFrame(RelativeDateRangeValue(Quarter, -3)),
      Seq("last 4 quarters") -> timeFrame(RelativeDateRangeValue(Quarter, -4)),
      Seq("this quarter") -> timeFrame(RelativeDateRangeValue(Quarter, 0)),
      Seq("next quarter") -> timeFrame(RelativeDateRangeValue(Quarter, +1)),
      Seq("next 2 quarters") -> timeFrame(RelativeDateRangeValue(Quarter, +2)),
      Seq("next 3 quarters") -> timeFrame(RelativeDateRangeValue(Quarter, +3)),
      Seq("next 4 quarters") -> timeFrame(RelativeDateRangeValue(Quarter, +4)),
      Seq("last fiscal quarter") -> timeFrame(RelativeDateRangeValue(Fiscal_Quarter, -1)),
      Seq("last 2 fiscal quarters") -> timeFrame(RelativeDateRangeValue(Fiscal_Quarter, -2)),
      Seq("last 3 fiscal quarters") -> timeFrame(RelativeDateRangeValue(Fiscal_Quarter, -3)),
      Seq("last 4 fiscal quarters") -> timeFrame(RelativeDateRangeValue(Fiscal_Quarter, -4)),
      Seq("this fiscal quarter") -> timeFrame(RelativeDateRangeValue(Fiscal_Quarter, 0)),
      Seq("next fiscal quarter") -> timeFrame(RelativeDateRangeValue(Fiscal_Quarter, +1)),
      Seq("next 2 fiscal quarters") -> timeFrame(RelativeDateRangeValue(Fiscal_Quarter, +2)),
      Seq("next 3 fiscal quarters") -> timeFrame(RelativeDateRangeValue(Fiscal_Quarter, +3)),
      Seq("next 4 fiscal quarters") -> timeFrame(RelativeDateRangeValue(Fiscal_Quarter, +4)),
      Seq("last week") -> timeFrame(RelativeDateRangeValue(Week, -1)),
      Seq("this week") -> timeFrame(RelativeDateRangeValue(Week,  0)),
      Seq("next week") -> timeFrame(RelativeDateRangeValue(Week, +1)),
      Seq("yesterday") -> timeFrame(RelativeDateRangeValue(Day, -1)),
      Seq("last 2 days") -> timeFrame(RelativeDateRangeValue(Day, -2)),
      Seq("last 3 days") -> timeFrame(RelativeDateRangeValue(Day, -3)),
      Seq("last 4 days") -> timeFrame(RelativeDateRangeValue(Day, -4)),
      Seq("last 5 days") -> timeFrame(RelativeDateRangeValue(Day, -5)),
      Seq("last 6 days") -> timeFrame(RelativeDateRangeValue(Day, -6)),
      Seq("last 7 days") -> timeFrame(RelativeDateRangeValue(Day, -7)),
      Seq("last 8 days") -> timeFrame(RelativeDateRangeValue(Day, -8)),
      Seq("last 9 days") -> timeFrame(RelativeDateRangeValue(Day, -9)),
      Seq("last 10 days") -> timeFrame(RelativeDateRangeValue(Day, -10)),
      Seq("last 11 days") -> timeFrame(RelativeDateRangeValue(Day, -11)),
      Seq("last 12 days") -> timeFrame(RelativeDateRangeValue(Day, -12)),
      Seq("last 13 days") -> timeFrame(RelativeDateRangeValue(Day, -13)),
      Seq("last 14 days") -> timeFrame(RelativeDateRangeValue(Day, -14)),
      Seq("last 15 days") -> timeFrame(RelativeDateRangeValue(Day, -15)),
      Seq("last 16 days") -> timeFrame(RelativeDateRangeValue(Day, -16)),
      Seq("last 17 days") -> timeFrame(RelativeDateRangeValue(Day, -17)),
      Seq("last 18 days") -> timeFrame(RelativeDateRangeValue(Day, -18)),
      Seq("last 19 days") -> timeFrame(RelativeDateRangeValue(Day, -19)),
      Seq("last 20 days") -> timeFrame(RelativeDateRangeValue(Day, -20)),
      Seq("last 21 days") -> timeFrame(RelativeDateRangeValue(Day, -21)),
      Seq("last 22 days") -> timeFrame(RelativeDateRangeValue(Day, -22)),
      Seq("last 23 days") -> timeFrame(RelativeDateRangeValue(Day, -23)),
      Seq("last 24 days") -> timeFrame(RelativeDateRangeValue(Day, -24)),
      Seq("last 25 days") -> timeFrame(RelativeDateRangeValue(Day, -25)),
      Seq("last 26 days") -> timeFrame(RelativeDateRangeValue(Day, -26)),
      Seq("last 27 days") -> timeFrame(RelativeDateRangeValue(Day, -27)),
      Seq("last 28 days") -> timeFrame(RelativeDateRangeValue(Day, -28)),
      Seq("last 29 days") -> timeFrame(RelativeDateRangeValue(Day, -29)),
      Seq("last 30 days") -> timeFrame(RelativeDateRangeValue(Day, -30)),
      Seq("last 60 days") -> timeFrame(RelativeDateRangeValue(Day, -60)),
      Seq("last 90 days") -> timeFrame(RelativeDateRangeValue(Day, -90)),
      Seq("last 120 days") -> timeFrame(RelativeDateRangeValue(Day, -120)),
      Seq("last 180 days") -> timeFrame(RelativeDateRangeValue(Day, -180)),
      Seq("today") -> timeFrame(RelativeDateRangeValue(Day, 0)),
      Seq("tomorrow") -> timeFrame(RelativeDateRangeValue(Day, 1)),
      Seq("next 2 days") -> timeFrame(RelativeDateRangeValue(Day, +2)),
      Seq("next 3 days") -> timeFrame(RelativeDateRangeValue(Day, +3)),
      Seq("next 4 days") -> timeFrame(RelativeDateRangeValue(Day, +4)),
      Seq("next 5 days") -> timeFrame(RelativeDateRangeValue(Day, +5)),
      Seq("next 6 days") -> timeFrame(RelativeDateRangeValue(Day, +6)),
      Seq("next 7 days") -> timeFrame(RelativeDateRangeValue(Day, +7)),
      Seq("next 8 days") -> timeFrame(RelativeDateRangeValue(Day, +8)),
      Seq("next 9 days") -> timeFrame(RelativeDateRangeValue(Day, +9)),
      Seq("next 10 days") -> timeFrame(RelativeDateRangeValue(Day, +10)),
      Seq("next 11 days") -> timeFrame(RelativeDateRangeValue(Day, +11)),
      Seq("next 12 days") -> timeFrame(RelativeDateRangeValue(Day, +12)),
      Seq("next 13 days") -> timeFrame(RelativeDateRangeValue(Day, +13)),
      Seq("next 14 days") -> timeFrame(RelativeDateRangeValue(Day, +14)),
      Seq("next 15 days") -> timeFrame(RelativeDateRangeValue(Day, +15)),
      Seq("next 16 days") -> timeFrame(RelativeDateRangeValue(Day, +16)),
      Seq("next 17 days") -> timeFrame(RelativeDateRangeValue(Day, +17)),
      Seq("next 18 days") -> timeFrame(RelativeDateRangeValue(Day, +18)),
      Seq("next 19 days") -> timeFrame(RelativeDateRangeValue(Day, +19)),
      Seq("next 20 days") -> timeFrame(RelativeDateRangeValue(Day, +20)),
      Seq("next 21 days") -> timeFrame(RelativeDateRangeValue(Day, +21)),
      Seq("next 22 days") -> timeFrame(RelativeDateRangeValue(Day, +22)),
      Seq("next 23 days") -> timeFrame(RelativeDateRangeValue(Day, +23)),
      Seq("next 24 days") -> timeFrame(RelativeDateRangeValue(Day, +24)),
      Seq("next 25 days") -> timeFrame(RelativeDateRangeValue(Day, +25)),
      Seq("next 26 days") -> timeFrame(RelativeDateRangeValue(Day, +26)),
      Seq("next 27 days") -> timeFrame(RelativeDateRangeValue(Day, +27)),
      Seq("next 28 days") -> timeFrame(RelativeDateRangeValue(Day, +28)),
      Seq("next 29 days") -> timeFrame(RelativeDateRangeValue(Day, +29)),
      Seq("next 30 days") -> timeFrame(RelativeDateRangeValue(Day, +30)),
      Seq("next 60 days") -> timeFrame(RelativeDateRangeValue(Day, +60)),
      Seq("next 90 days") -> timeFrame(RelativeDateRangeValue(Day, +90)),
      Seq("next 120 days") -> timeFrame(RelativeDateRangeValue(Day, +120)),
      Seq("next 180 days") -> timeFrame(RelativeDateRangeValue(Day, +180)),
      /*
      Seq("quarter") -> List(
        (Noun, Done(BaseColumn("lead", "t.choose_date.quarter")))
      ), */
      Seq("day", "date") -> List((Noun, Done(BaseColumn("_", "date(t.$prominentDateColumn)", false)))),
      Seq("daily") -> List((Adjective, argMatch[LFNode](1, { case (column: ColumnOp) :: Nil => AggregateBy(column, Some(BaseColumn("_", "date(t.$prominentDateColumn)", false))) }))),
      Seq("month") -> List(
        (Noun, Done(BaseColumn("_", "strftime('%Y-%m', t.$prominentDateColumn)", false)))
      ),
      Seq("monthly") -> List(
        (Adjective, argMatch[LFNode](1, { case (column: ColumnOp) :: Nil => AggregateBy(column, Some(BaseColumn("_", "strftime('%m-%Y', t.$prominentDateColumn)", false))) }))
      ),
      Seq("year") -> List(
        (Noun, Done(BaseColumn("_", "strftime('%Y', t.$prominentDateColumn)", false)))
      ),
      Seq("yearly") -> List(
        (Adjective, argMatch[LFNode](1, { case (column: ColumnOp) :: Nil => AggregateBy(column, Some(BaseColumn("_", "strftime('%Y', t.$prominentDateColumn)", false))) }))
      ),
      /*
       * "recently created opportunities", in this case "recently" is the driving word and consumes first
       * "created" and then "opportunities", then applies a filter etc.
       * @todo we should check that column is actually a date column
       */
      Seq("recently") -> List(
        (ForwardCat(ForwardCat(Noun, Noun), Adjective),
          lift2[LFNode]({ case column: BaseColumn =>
            { case rel: RelationLike =>
              OrganizeBy(Filter(rel, BasePredicate(column.tableName, s"t.${column.columnName} >= LAST_N_DAYS:30")), column, Desc) } }))
      )
    )
}


/**
 * Token matchers
 * (I believe these are Token matchers, not ParseToken matchers)
 * @todo Also add these to StarDict?
 * @todo These should be turned into a flattenedDict too, so they are easily added to StarDict.
 */
object TokenMatchers extends DictHelpers {
  type MatchFunction = String => List[(CcgCat, SemanticState)]

  // change this to match true or false, and specify the lexical and semantic forms in the dictionarys
  private[this] val numericLiteral = { str: String =>
    try {
      List(
        (Adjective, Done(Literal(Integer.parseInt(str)))),
        (Noun, Done(Literal(Integer.parseInt(str))))
      )
    } catch {
      case nfe: NumberFormatException => Nil
    }
  }

  private[this] val numericTokens = Map(
    "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9, "ten" -> 10,
    "eleven" -> 11, "twelve" -> 12, "thirteen" -> 13, "fourteen" -> 14, "fifteen" -> 15, "sixteen" -> 16, "seventeen" -> 17, "eighteen" -> 18, "nineteen" -> 19, "twenty" -> 20
  )

  private[this] val numericTokensMatcher = { str: String =>
    numericTokens.get(str) map { num => (Adjective, Done(Literal(num))) } toList
  }
  
  private[this] val yearMatcher: MatchFunction = { str: String =>
    try {
      val numericValue = Integer.parseInt(str)
      if (numericValue >= 1900 && numericValue < 2200) {
        timeFrame(AbsoluteDatePeriodValue(sqlkit.Year, numericValue))
      } else {
        Nil
      }
    } catch {
      case nfe: NumberFormatException => Nil
    }
  }

  val matchers: Seq[MatchFunction] = Seq(numericLiteral, numericTokensMatcher, yearMatcher)
}


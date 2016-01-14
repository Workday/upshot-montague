package sfdc

import natural._
import scala.collection.mutable
import sqlkit.{SqlVariable, BinaryPredicate}

/**
 * Salesforce-specific dictionary generation
 */
object SfdcMetadict {

  def generateDict(md: SfdcMetadata, tablesToInclude: Set[String] = Set.empty, forStarDict: Boolean = false): NQDict = {
    val map = mutable.Map[Seq[String], List[(CcgCat, SemanticState)]]()

    /**
     * The predicate-generating function for "my":  Filter on t.ownerId for
     * any table t e.g. "my accounts", "my opportunities", "my open opportunities", etc.
     * @todo Create a helper function to instantiate BinaryPredicate's from strings
     */
    val myPredicateFn = makePredicateFunctionFromTemplate(md,
      BinaryPredicate("t.$ownerColumn", "=", List(SqlVariable("userId"))),
      "$ownerColumn",
      table => table.getOwnerColumn.map(_.name)
    )
    putOrAdd(map, Seq("my") -> List(
      (Adjective, lift[LFNode]({ case rel: RelationLike => Filter(rel, DynamicPredicate(myPredicateFn)) }))
    ))

    map.toMap
  }

  /**
   * Return a function that, given a template binary predicate like
   *
   *    t.$ownerColumn = 'abcdef'
   *
   * Returns a predicate-generating function that will translate the binary predicate to a concrete predicate
   * where the token $ownerColumn is replaced by something else.
   *
   * @param md the metadata to be stored as "hidden state" inside the returned function
   * @param templatePred the binary predicate to serve as a template for the predicate to be generated
   * @param token the token to replace in the template
   * @param replacerFn a function that generates the replacement for the token, taking as input the relation
   *                   against which this predicate is being applied
   * @return a function that can be used in a nqlf DynamicPredicate
   */
  private[this] def makePredicateFunctionFromTemplate(md: SfdcMetadata, templatePred: BinaryPredicate, token: String,
                                                  replacerFn: SfdcTable => Option[String]): RelationLike => Option[BasePredicate] = {
    rel: RelationLike =>
      SqlGen.findUnderlyingTableName(rel) flatMap { tableName =>
        md.getTable(tableName) flatMap { table =>
          replacerFn(table) map { replacement =>
            val expr2 = templatePred.transformColumnSql(_.replaceAllLiterally(token, replacement))
            BasePredicate(tableName, expr2)
          }
        }
      }
  }

  // TODO: move this to a util.collection package.  scala doesn't have ListMultiMap
  private[this] def putOrAdd[A, B](map: mutable.Map[A, List[B]], entry: (A, List[B])) = {
    if (map.contains(entry._1)) map(entry._1) ++= entry._2 else map += entry
  }


}
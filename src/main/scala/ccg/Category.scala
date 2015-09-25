package ccg

/**
 * Helper functions relating to CCG categories.
 */
object Category {
  def matches(category: String, targetCategory: String): Boolean = {
    (category, targetCategory) match {
      case ("N", "NP") => true  // special case: an N is an NP
      case (x, y) => x == y
    }
  }
}

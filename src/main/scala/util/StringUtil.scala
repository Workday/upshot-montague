package util

/**
 * Utility functions for strings.
 */
object StringUtil {
  /**
   * Turns a parenthetical string (e.g. "are(writing(a(parser)))(and(joseph)(alex))")
   * into a pretty tree representation.
   */
  def toPrettyTree(tree: String) = {
    var indent = 2
    val str = new StringBuilder("\n" + " " * indent)

    tree.foreach {
      case ')' => indent -= 2
      case '(' => indent += 2; str.append("\n" + " " * indent)
      case char => str.append(char);
    }

    str.toString()
  }
}

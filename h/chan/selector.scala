import scala.util.matching.Regex.Match
import scala.util.matching.Regex

// package h.chan.selector

object Selector {
  // REGEX: /*comment*/ |\ffsd | \# | .
  final val REGEX = new Regex("""(/\*.*?\*/)|\\(?:([0-9a-fA-F]{1,6}\s)|([/,\s>+~#.:\[])|.)""",
    "comment", "hexDigits", "specialChar"
  )

  // consider conditional normalization:
  // normalize only when / or \\ exists
  def apply(source: String): SelectorList = new SelectorList(normalize(source))

  def normalize(source: String) = REGEX.replaceAllIn(source, replacer _)

  private def replacer(m: Match): String = {
    val Seq(comment, hexDigits, specialChar) = m.subgroups
    if (comment != null) return ""
    val hex: String =
      if (hexDigits != null) hexDigits.dropRight(1)
      else if (specialChar != null) specialChar(0).toHexString
      else null

    if (hex == null) return m.matched
    val nextChar = m.source.charAt(m.end).toString
    if (nextChar matches "[0-9a-fA-F]") "\\" + ("000000" + hex).takeRight(6)
    else if (nextChar matches """\s""") "\\" + hex + " "
    else "\\" + hex

    // consider using Regex.unapplySeq
    // final val HEX_REGEX = "[0-9a-fA-F]".r
    // final val BLANK_REGEX = "\\s".r
    // if (HEX_REGEX.unapplySeq(nextChar).isDefined)
    //   "\\" + ("000000" + hex).takeRight(6)
    // else if (BLANK_REGEX.unapplySeq(nextChar).isDefined)
    //   "\\" + hex + " "
  }
}

sealed abstract class AbstractSelector(source: String) {
  override def toString: String = source

  def containsSelector(selector: AbstractSelector): Boolean
  def contains(selector: AbstractSelector): Boolean = containsSelector(selector)
  def contains(selectorSource: String): Boolean =
    containsSelector(Selector(selectorSource))
}

class SelectorList(source: String) extends AbstractSelector(a) {
  // consider lazy
  private val list = source.split(",").map(new ComplexSelector(_))
  override def toString: String = list.map(_.toString).mkString(", ")

  def containsSelector(selector: AbstractSelector) =
    list.exists(_ containsSelector selector)
}

class ComplexSelector(sources: Array[String])
  extends AbstractSelector(sources.mkString(" ")) {

  require(sources.length > 0)
  val x: ComplexSelector = new CompoundSelector(sources.last)
  val combinator: Option(String =
    if (sources.length) > 1 source.init.last
    else

  // workaround for js's split
  pirvate final val SELECTOR_SPLITER = """(?=[>+~])|(?<=[>+~])|\s+"""
  private final val CLEANER = """\s*([>+~])\s*""".r
  def this(source: String) = {
    require(source.indexOf(',') < 0)
    // ?? +~ should not appear here
    val cleaned = CLEANER.replaceAllIn(source, "$1")
    this(cleaned.trim.split(SELECTOR_SPLITER))
  }
}

import scala.util.matching.Regex.Match
import scala.util.matching.Regex
import scala.annotation.{switch}

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
      if (hexDigits != null) hexDigits.init
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

class SelectorList(source: String) extends AbstractSelector(source) {
  // consider lazy
  // guarantee ComplexSelector contains ComplexSelector
  private val list = source.split(",").map(new ComplexSelector(_))
  override def toString: String = list.map(_.toString).mkString(", ")

  def containsSelector(selector: AbstractSelector) =
    list.exists(_ containsSelector selector)
}

// add companion object to allow function
private object ComplexSelector {
  // modifier must be object/class level
  private final val SELECTOR_SPLITER = """(?=[>+]|~(?!=))|(?<=[>+]|~(?!=))|(?=\s+)|(?<=\s+)"""
  private final val CLEANER = """\s*([>+~])\s*""".r
  private final val TRIMMER = """\s+""".r

  def cleanSelector(source: String): Array[String] = {
    // workaround for js's split
    require(source.indexOf(',') < 0)
    // ?? +~ should not appear here
    val cleaned = CLEANER.replaceAllIn(source, "$1")
    val trimmed = TRIMMER.replaceAllIn(cleaned, " ").trim
    trimmed.split(SELECTOR_SPLITER)
  }
}

class ComplexSelector(sources: Array[String])
  extends AbstractSelector(sources.mkString("")) {

  require(sources.length > 0)
  val x: CompoundSelector = new CompoundSelector(sources.last)
  val combinator: String =
    if (sources.length > 1) sources.init.last
    else null
  val xs: ComplexSelector =
    if (sources.length > 2) new ComplexSelector(sources.dropRight(2))
    else if (sources.length > 1) new ComplexSelector(Array(""))
    else null


  def this(source: String) = {
    // this must be the first statement
    // to avoid recursion
    this(ComplexSelector.cleanSelector(source))
  }

  private def findSubSelector(combinators: Seq[String], selector: ComplexSelector): Boolean = {
    var otherXs = selector.xs
    var r: Boolean = false

    // complexity: n^2
    while (true) { // loop1
      if (otherXs == null) return false
      r = xs.contains(otherXs) // loop2
      if (r) return r
      var c: String= null
      // skip sibling selector
      do {
        c = otherXs.combinator
        if (c == null) return false
        otherXs = otherXs.xs
        if (otherXs == null) return false
      } while (!combinators.contains(c))
    }
    false
  }

  def containsSelector(selector: AbstractSelector): Boolean = selector match {
    case s: ComplexSelector =>
      var r = x.contains(s.x)
      if (!r) return r

      // guarantee contains call against ComplexSelector
      if (combinator == null) return r

      (combinator, s.combinator) match {
        case (" ", ">") =>
          findSubSelector(Seq(" ", ">"), s)
        case ("~", "+") =>
          findSubSelector(Seq("~", "+"), s)
        case (a, b) if a != b =>
          false
        case _ =>
          (xs == null) || xs.contains(s.xs)
      }
    case _ =>
      // should not reach here
      false
  }
}

class CompoundSelector(source: String) extends AbstractSelector(source) {
  require(!(source matches "[, >+]|~(?!="))
  private final val SPLITER = """(?=[#\.:\[]])|(?<=[#\.:\[])""".r
  import collection.Iterator

  @transient
  private val normalizedSource =
    if ("#.:[" contains source(0)) "*" + source
    else source
  private val a = SPLITER.split(normalizedSource)
  val tpe: ElementalSelector = new ElementalSelector(a.head)
  val simpleSelectors: Iterator[SimpleSelector] = a.tail.grouped(2).map {arr =>
    SimpleSelector(arr(0), arr(1))
  }

  def containsSelector(selector: AbstractSelector): Boolean = selector match {
    case s: CompoundSelector =>
      val r = this.tpe.contains(s.tpe)
      if (!r) return r
      simpleSelectors.forall {s0 =>
        s.simpleSelectors.exists {s1 =>
          s0.contains(s1)
        }
      }
    case _ => false
  }
}

sealed abstract class SimpleSelector(x: String, xs: String)
  extends AbstractSelector(x + xs)

object SimpleSelector {
  def apply(x: String, xs: String): SimpleSelector = (x: @switch) match {
    case "#"  => new IDSelector(xs)
    case "." => new ClassSelector(xs)
    case ":" => new PsuedoClass(xs)
    case "[" =>
      require(xs.last == ']')
      new AttributeSelector(xs.init)
    case _ => new ElementalSelector(x)
  }
}

class IDSelector(val id: String) extends SimpleSelector("#", id) {
  def containsSelector(selector: AbstractSelector): Boolean = selector match {
    case s: IDSelector => id == s.id
    case _ => false
  }
}

class ClassSelector(val cls: String) extends SimpleSelector(".", cls) {
  def containsSelector(selector: AbstractSelector) = selector match {
    case s: ClassSelector => cls == s.cls
    case _ => false
  }
}

// BULLSHIT
class AttributeSelector(source: String) extends SimpleSelector("[", source) {
  private final val Attr = """^(.*?)(?:([~|^$*]?=)(['"]?)(.*)\3)?$""".r
  val Attr(attr, rel, _, value) = source

  override def toString: String = "[" + source + "]"

  def containsSelector(selector: AbstractSelector): Boolean = selector match {
    case s: AttributeSelector =>
      if (attr != s.attr) false
      else if (rel == null) true
      else if (rel == s.rel && value == s.value) true
      else if (s.rel == null) false
      else (rel: @switch) match {
          case "~=" =>
            val otherVals = s.value.split("\\s+")
            (s.rel: @switch) match {
              case "=" => otherVals.contains(value)
              case "^=" => otherVals.init.contains(value)
              case "$=" => otherVals.tail.contains(value)
              case "*=" => otherVals.tail.init.contains(value)
              case _ => false
            }
          case "|=" =>
            "=" == s.rel && (value == s.value || s.value.take(value.length+1) == value + "-") ||
            ("^=" == s.rel || s.value.take(value.length+1) == value + "-")
          case "^=" => (!value.isEmpty) &&
            ("=" == s.rel || "|=" == s.rel || "^=" == s.rel) &&
            (value == s.value.take(value.length))
          case "$=" => (!value.isEmpty) &&
            ("=" == s.rel || "$=" == s.rel) &&
            (value == s.value.takeRight(value.length))
          case "*=" =>
            (!value.isEmpty) && s.value != null && s.value.contains(value)
          case _ =>
            false
      }
  }
}

class ElementalSelector(n: String) extends SimpleSelector("", n) {
  val name = if (n == null || n.isEmpty) "*" else n
  @inline private def isUniversal = name == "*"

  def containsSelector(selector: AbstractSelector): Boolean = isUniversal || (selector match {
    case s: ElementalSelector => name == s.name
    case _ => isUniversal
  })
}

class PsuedoClass(source: String) extends SimpleSelector(":", source) {
  def containsSelector(selector: AbstractSelector): Boolean = ???
}

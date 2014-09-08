// Refactor:
// change AbstractSelector to HigherKinded
package h.chan.selector

import scala.util.matching.Regex.Match
import scala.util.matching.Regex
import scala.annotation.{switch}

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

  def containsSelector(selector: AbstractSelector) = selector match {
    case sl: SelectorList =>
      sl.list.forall(s => list.exists {sel => sel contains s})
    case _ =>
      false
  }
}

// add companion object to allow function
private object ComplexSelector {
  // modifier must be object/class level
  private final val SELECTOR_SPLITER = """(?=>|~(?!=)|\+(?!\d|n\)))|(?<=>|~(?!=)|\+(?!\d|n\)))|(?=\s+)|(?<=\s+)"""
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
  require(("""[, >]|~(?!=)|\+(?!\d|n\))""".r findFirstIn source) match {
    case None => true
    case _ => false
  })
  private final val SPLITER = """(?=[#\.:\[])|(?<=[#\.:\[])""".r

  @transient
  private val normalizedSource =
    if ("#.:[" contains source(0)) "*" + source
    else source
  private val a = SPLITER.split(normalizedSource)
  val tpe: ElementalSelector = new ElementalSelector(a.head)
  // grouped return Iterator, which can be only TraverseOnce
  // and exists nested in forall will Traverse several times
  // which is undefined behavior
  val simpleSelectors: List[SimpleSelector] = a.tail.grouped(2).map {arr =>
    SimpleSelector(arr(0), arr(1))
  }.toList

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
  // optimization: use table switch
  def apply(x: String, xs: String): SimpleSelector = (x(0): @switch) match {
    case '#'  => new IDSelector(xs)
    case '.' => new ClassSelector(xs)
    case ':' => PsuedoClass(xs)
    case '[' =>
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
  // scala matches and only matches whole string, as Java does
  // so ^$ anchors are redundant
  private final val Attr = """^(.*?)(?:([~|^$*]?=)(['"]?)(.*)\3)?$""".r
  val Attr(attr, rel, _, value) = source

  override def toString: String = "[" + source + "]"

  def containsSelector(selector: AbstractSelector): Boolean = selector match {
    case s: AttributeSelector =>
      if (attr != s.attr) false
      else if (rel == null) true
      else if (rel == s.rel && value == s.value) true
      else if (s.rel == null) false
      else rel match {
          case "~=" =>
            val otherVals = s.value.split("\\s+")
            s.rel match {
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
    case _ =>
      false
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

abstract class PsuedoClass(source: String) extends SimpleSelector(":", source)

class NthPC(pc: String, source: String) extends PsuedoClass(NthPC.format(pc, source)) {
  val last: Boolean = pc matches "last-.*"
  val child: Boolean = pc matches ".*?-child"
  var a: Int = 0
  var b: Int = 1
  // optional B
  private final val `An+B` = """([+-]?\d?)n([+-]\d+)?""".r
  // optional An
  private final val `B+An` = """([+-]?\d+)(?:([+-]\d?)n)?""".r

  (source.replace(" ", "")) match {
    case "even" =>
      a = 2
      b = 0
    case "odd" =>
      a = 2
      b = 1
    case `An+B`(a_, b_) =>
      a = a_ match {
        case "" | "+" => 1
        case "-" => -1
        case _ => a_.toInt
      }
      b = if (b_ == null) 0 else b_.toInt
    case `B+An`(b_, a_) =>
      b = b_.toInt
      a = a_ match {
        case "+" => 1
        case "-" => -1
        case _: String => a_.toInt
        case _ => 0
      }
    case _ =>
  }

  def containsSelector(selector: AbstractSelector): Boolean = selector match {
    case s: NthPC =>
      if (last != s.last) return false
      if (child != s.child) {
        // note that child and type are mutually exclusive
        // first-type -> first-child, but not child -> type
        return s.child && a == 0 && b == 1 && s.a == 0 && s.b == 1
      }
      if (s.a == 0) {
        if (a == 0) return b == s.b
        val na = s.b - b
        return (na == 0) || (
          (na % a == 0) && (na * a > 0)
        )
      }
      if (s.a > 0) {
        if (a <= 0 || (s.a % a != 0))  return false
        val na = s.b - b
        return (na % a) == 0 && firstNat(a, b) <= firstNat(s.a, s.b)
      }

      if (a == 0) return s.b == b
      lazy val stream: Stream[Int]= 0 #:: (stream.map(_+1))
      stream.map(_ * s.a + s.b).takeWhile(_ > 0).forall {e =>
        val na = e - b
        (na == 0) || (na % a) == 0 && (na * a > 0)
      }
    case _ => false
  }

  @inline private def firstNat(a: Int, b: Int) =
    if (b > 0) b else b % a + a
}

private object NthPC {
  @inline def format(pc: String, source: String) = {
    if (source.isEmpty) pc
    else s"$pc($source)"
  }
}

object PsuedoClass {
  private final val PsuedoDef = """^(.+?)(?:\((.*?)\))?$""".r
  def apply(source: String): PsuedoClass = {
    val PsuedoDef(constructor, argument) = source
    constructor match {
      case "nth-child" | "nth-last-child" |"nth-of-type" | "nth-last-of-type" =>
        new NthPC(constructor, argument)
      case "first-child" | "first-of-type" | "last-child" | "last-of-type"  =>
        new NthPC(constructor, "")
      case _ => sys.error("unsupported type")
    }
  }

}
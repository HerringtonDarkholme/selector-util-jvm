// Refactor:
// change AbstractSelector to HigherKinded
// Update: no performance gain, but it is idiomatic
package h.chan.selector

import scala.util.matching.Regex.Match
import scala.util.matching.Regex
import scala.annotation.{switch}

object Selector {
  // REGEX: /*comment*/ |\ffsd | \# | .
  final val REGEX = new Regex("""(/\*.*?\*/)|\\(?:([0-9a-fA-F]{1,6}\s)|([/,\s>+~#.:\[])|.)""",
    "comment", "hexDigits", "specialChar"
  )

  // TODO: handle error
  def apply(source: String): SelectorList = {
    CSSParser.parseAll(CSSParser.ListParser, normalize(source)).get
  }

  // consider conditional normalization:
  // normalize only when / or \\ exists
	// Upadte: Performance improvment confirmed
  def normalize(source: String) =
		if (source.indexOf("\\") < 0 && source.indexOf("/") < 0) source
		else	REGEX.replaceAllIn(source, replacer _)

  // consider using Regex.unapplySeq
  // Update: no performance gain
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
  }
}

abstract class AbstractSelector[S <: AbstractSelector[S]](source: String) {
  override def toString: String = source

  def containsSelector(selector: S): Boolean
  def contains(selector: S): Boolean = containsSelector(selector)
}

case class SelectorList(list: List[ComplexSelector])
	extends AbstractSelector[SelectorList](list.mkString(", ")) {
  // consider lazy
  // guarantee ComplexSelector contains ComplexSelector

  def containsSelector(sl: SelectorList) =
    sl.list.forall(s => list.exists {sel => sel contains s})

  def contains(selectorSource: String): Boolean =
    containsSelector(Selector(selectorSource))
}

// add companion object to allow function

private object ComplexSelector {
  def apply(cpd: CompoundSelector): ComplexSelector = {
    ComplexSelector('\0', cpd, None)
  }
}

case class ComplexSelector(combinator: Char, x: CompoundSelector, xs: Option[ComplexSelector])
  extends AbstractSelector[ComplexSelector]((xs match {
    case Some(c) => c + combinator.toString + x
    case _ => x.toString
  })) {

  def apply(combinator: Char, compound: CompoundSelector): ComplexSelector =
    new ComplexSelector(combinator, compound, Some(this))

  // private def findSubSelector(combinators: Seq[Char], selector: Option[ComplexSelector]): Boolean = { selector match {
  private def findSubSelector(combinators: Seq[Char], selector: ComplexSelector): Boolean = {
  // selector match {
  //   case Some(s) =>
  //     if (!combinators.contains(s.combinator)) findSubSelector(combinators, s.xs)
  //     else xs.get.containsSelector(s)
  //   case _ => false
  // }

    var otherXs = selector.xs.getOrElse(null)
    var r: Boolean = false

    // complexity: n^2
    while (true) { // loop1
      if (otherXs == null) return false
      r = xs.get.contains(otherXs) // loop2
      if (r) return r
      var c: Char = '\0'
      // skip sibling selector
      while (!combinators.contains(c)) {
        c = otherXs.combinator
        if (c == '\0') return false
        otherXs = otherXs.xs.getOrElse(null)
        if (otherXs == null) return false
      }
    }
    false
  }

  def containsSelector(s: ComplexSelector): Boolean = {
		var r = x.contains(s.x)
		if (!r) return r

		// guarantee contains call against ComplexSelector
		if (combinator == '\0') return r

		(combinator: @switch) match {
			case ' ' =>
				findSubSelector(Seq(' ', '>'), s)
			case '~' =>
				findSubSelector(Seq('~', '+'), s)
			case _ =>
				xs.get.containsSelector(s.xs.get)
		}
  }
}

case class CompoundSelector(simpleSelectors: List[SimpleSelector])
  extends AbstractSelector[CompoundSelector](simpleSelectors.mkString("")) {

  val tpe: TypeSelector = new TypeSelector("*")

  def containsSelector(s: CompoundSelector): Boolean =  {
		val r = this.tpe.contains(s.tpe)
		if (!r) return r
		simpleSelectors.forall {s0 =>
			s.simpleSelectors.exists {s1 =>
				s0.contains(s1)
			}
		}
  }
}

abstract class SimpleSelector(x: String, xs: String)
  extends AbstractSelector[SimpleSelector](x + xs)

case class IDSelector(val id: String) extends SimpleSelector("#", id) {
  def containsSelector(selector: SimpleSelector): Boolean = selector match {
    case s: IDSelector => id == s.id
    case _ => false
  }
}

case class ClassSelector(val cls: String) extends SimpleSelector(".", cls) {
  def containsSelector(selector: SimpleSelector) = selector match {
    case s: ClassSelector => cls == s.cls
    case _ => false
  }
}

// BULLSHIT
case class AttributeSelector(attr: String, rel: Char, value: String) extends SimpleSelector("[", attr+rel+value+"]") {
  // scala matches and only matches whole string, as Java does
  // so ^$ anchors are redundant
  // private final val Attr = """^(.*?)(?:([~|^$*]?=)(['"]?)(.*)\3)?$""".r
  // val Attr(attr, rel, _, value) = source

  // override def toString: String = "[" + source + "]"

  def containsSelector(selector: SimpleSelector): Boolean = selector match {
    case s: AttributeSelector =>
      if (attr != s.attr) false
      else if (rel == '\0') true
      else if (rel == s.rel && value == s.value) true
      else if (s.rel == '\0') false
      else (rel: @switch) match {
          case '~' =>
            val otherVals = s.value.split("\\s+")
            (s.rel: @switch) match {
              case '=' => otherVals.contains(value)
              case '^' => otherVals.init.contains(value)
              case '$' => otherVals.tail.contains(value)
              case '*' => otherVals.tail.init.contains(value)
              case _ => false
            }
          case '|' =>
            '=' == s.rel && (value == s.value || s.value.take(value.length+1) == value + "-") ||
            ('^' == s.rel || s.value.take(value.length+1) == value + "-")
          case '^' => (!value.isEmpty) &&
            ('=' == s.rel || '|' == s.rel || '^' == s.rel) &&
            (value == s.value.take(value.length))
          case '$' => (!value.isEmpty) &&
            ('=' == s.rel || '$' == s.rel) &&
            (value == s.value.takeRight(value.length))
          case '*' =>
            (!value.isEmpty) &&  s.value.contains(value)
          case _ =>
            false
      }
    case _ =>
      false
  }
}

case class TypeSelector(n: String) extends SimpleSelector("", n) {
  val name = if (n == null || n.isEmpty) "*" else n
  @inline private def isUniversal = name == "*"

  def containsSelector(selector: SimpleSelector): Boolean = isUniversal || (selector match {
    case s: TypeSelector => name == s.name
    case _ => isUniversal
  })
}

abstract class PsuedoClass(source: String) extends SimpleSelector(":", source)

class NthPC(pc: String, source: String) extends PsuedoClass(NthPC.format(pc, source)) {
  val last: Boolean = pc matches "last-.*"
  val child: Boolean = pc matches ".*?-child"
  // optional B
  private final val `An+B` = """([+-]?\d?)n([+-]\d+)?""".r
  // optional An
  private final val `B+An` = """([+-]?\d+)(?:([+-]\d?)n)?""".r

  val (a, b) = (source.replace(" ", "")) match {
    case "even" =>
      (2, 0)
    case "odd" =>
      (2, 1)
    case `An+B`(a_, b_) =>
      (a_ match {
        case "" | "+" => 1
        case "-" => -1
        case _ => a_.toInt
      },
      if (b_ == null) 0 else b_.toInt)
    case `B+An`(b_, a_) =>
      (a_ match {
        case "+" => 1
        case "-" => -1
        case _: String => a_.toInt
        case _ => 0
      }, b_.toInt)
    case _ =>
      (0, 1)
  }

  def containsSelector(selector: SimpleSelector): Boolean = selector match {
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

case class NotPC(sels: SelectorList) extends PsuedoClass(sels.toString) {
  def containsSelector(sel: SimpleSelector) = true
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

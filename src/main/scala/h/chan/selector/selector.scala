// AbstractSelector is F-bounded Polymorphism
//  Update: no performance gain but idiomatic
package h.chan.selector

import scala.util.matching.Regex.Match
import scala.util.matching.Regex
import scala.annotation.{switch, tailrec}

object Selector {
  // REGEX: /*comment*/ |\ffsd | \# | .
  final val REGEX = new Regex("""(/\*.*?\*/)|\\(?:([0-9a-fA-F]{1,6}\s)|([/,\s>+~#.:\[])|.)""",
    "comment", "hexDigits", "specialChar"
  )

  // TODO: handle error
  def apply(source: String): SelectorList = {
    CSSParser.parseAll(CSSParser.ListParser, normalize(source)).get
  }

  // conditional normalization:
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

abstract class AbstractSelector[S <: AbstractSelector[S]] {
  def containsSelector(selector: S): Boolean
}

case class SelectorList(list: List[ComplexSelector])
	extends AbstractSelector[SelectorList] {

  def containsSelector(sl: SelectorList) =
    sl.list.forall(s => list.exists {sel => sel containsSelector s})

  def contains(selectorSource: String): Boolean =
    containsSelector(Selector(selectorSource))

  override def toString = list.mkString(", ")
}

class ComplexSelector(val combinator: Char, val x: CompoundSelector, val xs: ComplexSelector)
  extends AbstractSelector[ComplexSelector]() {

  @tailrec
  private final def findSubSelector(combinators: Seq[Char], s: ComplexSelector): Boolean = {
    if (s.combinator == '\u0000') false
    else if (!combinators.contains(s.combinator)) findSubSelector(combinators, s.xs)
    else containsSelector(s.xs) || findSubSelector(combinators, s.xs)
  }

  def containsSelector(s: ComplexSelector): Boolean = {
		var r = x.containsSelector(s.x)
		if (!r) return r

		if (combinator == '\u0000') return r

		(combinator: @switch) match {
			case ' ' => xs.findSubSelector(Seq(' ', '>'), s)
			case '~' => xs.findSubSelector(Seq('~', '+'), s)
			case _ =>
        if (s.combinator == '\u0000') false
        else xs.containsSelector(s.xs)
		}
  }

  override def toString = xs match {
    case _: ComplexSelector => xs + combinator.toString + x
    case _ => x.toString
  }
}

case class CompoundSelector(tpe: String, simpleSelectors: List[SimpleSelector])
  extends AbstractSelector[CompoundSelector] {

  def containsSelector(s: CompoundSelector): Boolean =  {
		if (tpe != "*" && tpe != s.tpe) false
    else simpleSelectors.forall {s0 =>
			s.simpleSelectors.exists {s1 =>
				s0.containsSelector(s1)
			}
		}
  }

  override def toString = tpe + simpleSelectors.mkString("")
}

abstract class SimpleSelector extends AbstractSelector[SimpleSelector]


case class IDSelector(val id: String) extends SimpleSelector {
  def containsSelector(selector: SimpleSelector): Boolean = selector match {
    case s: IDSelector => id == s.id
    case _ => false
  }

  override def toString = "#" + id
}

case class ClassSelector(val cls: String) extends SimpleSelector {
  def containsSelector(selector: SimpleSelector) = selector match {
    case s: ClassSelector => cls == s.cls
    case _ => false
  }
  override def toString = "." + cls
}

// BULLSHIT
case class AttributeSelector(attr: String, rel: Char, value: String) extends SimpleSelector {
  // scala matches and only matches whole string, as Java does
  // so ^$ anchors are redundant
  // private final val Attr = """^(.*?)(?:([~|^$*]?=)(['"]?)(.*)\3)?$""".r
  // val Attr(attr, rel, _, value) = source

  override def toString: String = "["+attr+rel+value+"]"

  def containsSelector(selector: SimpleSelector): Boolean = selector match {
    case s: AttributeSelector =>
      if (attr != s.attr) false
      else if (rel == '\u0000') true
      else if (rel == s.rel && value == s.value) true
      else if (s.rel == '\u0000') false
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


abstract class PsuedoClass extends SimpleSelector

class NthPC(pc: String, source: String) extends PsuedoClass {
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

  override def toString = ":" + NthPC.format(pc, source)
}

private object NthPC {
  @inline def format(pc: String, source: String) = {
    if (source.isEmpty) pc
    else s"$pc($source)"
  }
}

case class NotPC(sels: SelectorList) extends PsuedoClass {
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

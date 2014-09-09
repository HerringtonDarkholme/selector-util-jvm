package h.chan.selector

import scala.util.parsing.combinator.RegexParsers

object CSSParser extends RegexParsers {

  override def skipWhitespace = false

  def ListParser: Parser[List[ComplexSelector]] = repsep(ComplexParser, "\\s*,\\s*".r)

  private final val Combinator = """\s?[+>~]\s*""".r
  private final val Whitespace = """\s+""".r
  def ComplexParser: Parser[ComplexSelector] =
    (CompoundParser ~ Combinator ~ ComplexParser) ^^ {
      case cpd ~ cmb ~ cpx => cpx(cmb.trim, cpd)
    } |
    (CompoundParser ~ Whitespace ~ ComplexParser) ^^ {
      case cpd ~ _ ~ cpx => cpx(" ", cpd)
    }

  def CompoundParser: Parser[CompoundSelector] =
    (SimpleParser ~ CompoundParser) ^^ {
      case smp ~ cmp => cmp(smp)
    }

  def SimpleParser: Parser[SimpleSelector] =
    IDParser | ClassParser | PsuedoParser | AttributeParser | ElementalParser

  def IDParser: Parser[IDSelector] = ???

  def ClassParser: Parser[ClassSelector] = ???

  def PsuedoParser: Parser[PsuedoClass] = ???

  def AttributeParser: Parser[AttributeSelector] = ???

  def ElementalParser: Parser[ElementalSelector] = ???
}

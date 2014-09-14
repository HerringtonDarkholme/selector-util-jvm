package h.chan.selector

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.PackratParsers

object CSSParser extends RegexParsers  {

  override def skipWhitespace = false

  // refer to CSS spec
  // TODO: add namespace support

  // TODO: this is CSS2 spec
  // /\0-\277/.test('.') is right in javascript
  // private final val h = "[0-9a-fA-F]";
  // private final val unicode = """\\{h}{1,6}(\r\n|[ \t\r\n\f])?""".replace("{h}", h)
  // private final val escape = """({unicode}|\\[^\r\n\f0-9a-f])""".replace("{unicode}", unicode)
  // private final val nonascii = """[^\0000-\0277]"""
  // private final val nmchar = "([_A-Za-z0-9-]|{nonascii}|{escape})".replace("{nonascii}", nonascii).replace("{escape}", escape)
  // private final val nmstart = "([_A-Za-z]|{nonascii}|{escape})".replace("{nonascii}", nonascii).replace("{escape}", escape)
  // private final val ident = "-?{nmstart}{nmchar}*".replace("{nmstart}", nmstart).replace("{nmchar}", nmchar).r
  private final val ident = "[0-9A-Za-z_-]+".r

  def ListParser: Parser[SelectorList] = repsep(ComplexParser, "\\s*,\\s*".r) ^^ {
    case cmpList => SelectorList(cmpList)
  }

  private final val Combinator = """\s*[+>~]\s*""".r ^^ {
    case cmb => (cpx: ComplexSelector, cpd: CompoundSelector) =>
      new ComplexSelector(cmb.trim.head, cpd, cpx)
  }
  private final val Whitespace = """\s+""".r ^^ {
    case cmb => (cpx: ComplexSelector, cpd: CompoundSelector) =>
      new ComplexSelector(' ', cpd, cpx)
  }
  private final val ComplexZero = CompoundParser ^^ {
    case cpd =>
      new ComplexSelector('\u0000', cpd, null)
  }

  def ComplexParser: Parser[ComplexSelector] =
    chainl1(ComplexZero, CompoundParser, (Combinator|Whitespace))

  def CompoundParser: Parser[CompoundSelector] = (ident|"*") ~! rep(SimpleParser) ^^ {
    case tpe ~ sl =>
      CompoundSelector(tpe, sl)
  } |
  rep1(SimpleParser) ^^ {
    case sl =>
      CompoundSelector("*", sl)
  }

  def SimpleParser: Parser[SimpleSelector] =
    IDParser | ClassParser | PsuedoParser | AttributeParser


  def IDParser: Parser[AttributeSelector] = ("#" ~> ident) ^^ {
    case id => AttributeSelector("id", '=', id)
  }

  def ClassParser: Parser[AttributeSelector] = ("." ~> ident) ^^ {
    case id => AttributeSelector("class", '=', id)
  }

  def PsuedoParser: Parser[PsuedoClass] =
    // Parser Combinator does not recognize /.+?/
  (":" ~> ident ~ "(" ~! "[^)]+".r <~ ")" ) ^^ {
    case pc ~ _ ~ source => new NthPC(pc, source)
  } |
  (":" ~> ident) ^^ {
    case pc => new NthPC(pc, "")
  } |
  (":not(" ~> ListParser <~ ")") ^^ {
    case sels => NotPC(sels)
  }

  private final val rel = "[~^$*|]?=".r
  private final val str = """'[^']*(\\'[^']*)*'|"[^"]*(\\"[^"]*)*"""".r
  def AttributeParser: Parser[AttributeSelector] = ("[" ~> ident <~"]") ^^ {
    case attr =>
      AttributeSelector(attr, '\u0000', "")
  } |
  ("[" ~> ident ~! rel ~! (ident|str) <~ "]") ^^ {
    case attr ~ rel ~ value =>
      AttributeSelector(attr, rel.head, value)
  }

}

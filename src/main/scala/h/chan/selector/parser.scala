package h.chan.selector

import scala.util.parsing.combinator.RegexParsers

object CSSParser extends RegexParsers {

  override def skipWhitespace = false

  def ListParser: Parser[SelectorList] = repsep(ComplexParser, "\\s*,\\s*".r) ^^ {
    case cmpList => SelectorList(cmpList)
  }

  private final val Combinator = """\s*[+>~]\s*""".r
  private final val Whitespace = """\s+""".r
  def ComplexParser: Parser[ComplexSelector] =
    (CompoundParser ~ Combinator ~ ComplexParser) ^^ {
      case cpd ~ cmb ~ cpx =>
        cpx(cmb.trim, cpd)
    } |
    (CompoundParser ~ Whitespace ~ ComplexParser) ^^ {
      case cpd ~ _ ~ cpx =>
        cpx(" ", cpd)
    } |
    (CompoundParser) ^^ {
      case cpd =>
        ComplexSelector(cpd)
    }

  def CompoundParser: Parser[CompoundSelector] = rep1(SimpleParser) ^^ {
      case sl =>
        CompoundSelector(sl)
    }

  def SimpleParser: Parser[SimpleSelector] =
    IDParser | ClassParser | PsuedoParser | AttributeParser | TypeParser

  // refer to CSS spec
  // TODO: add namespace support

  // TODO: this is CSS2 spec
  // /\0-\277/.test('.') is right in javascript
  private final val h = "[0-9a-fA-F]";
  private final val unicode = """\\{h}{1,6}(\r\n|[ \t\r\n\f])?""".replace("{h}", h)
  private final val escape = """({unicode}|\\[^\r\n\f0-9a-f])""".replace("{unicode}", unicode)
  private final val nonascii = """[^\0000-\0277]"""
  private final val nmchar = "([_A-Za-z0-9-]|{nonascii}|{escape})".replace("{nonascii}", nonascii).replace("{escape}", escape)
  private final val nmstart = "([_A-Za-z]|{nonascii}|{escape})".replace("{nonascii}", nonascii).replace("{escape}", escape)
  private final val ident = "-?{nmstart}{nmchar}*".replace("{nmstart}", nmstart).replace("{nmchar}", nmchar).r
  // private final val ident = "[0-9A-z_-]+".r

  def IDParser: Parser[IDSelector] = ("#" ~> ident) ^^ {
    case id => IDSelector(id)
  }

  def ClassParser: Parser[ClassSelector] = ("." ~> ident) ^^ {
    case cls => ClassSelector(cls)
  }

  def PsuedoParser: Parser[PsuedoClass] =
    // Parser Combinator does not recognize /.+?/
  (":" ~> ident ~ "(" ~ "[^)]+".r <~ ")" ) ^^ {
    case pc ~ _ ~ source => new NthPC(pc, source)
  } | (":" ~> ident) ^^ {
    case pc => new NthPC(pc, "")
  } |
  (":not(" ~> ListParser <~ ")") ^^ {
    case sels => NotPC(sels)
  }

  private final val rel = "[~^$*|]?=".r
  private final val str = """'[^']*(\\'[^']*)*'|"[^"]*(\\"[^"]*)*"""".r
  def AttributeParser: Parser[AttributeSelector] = ("[" ~> ident <~"]") ^^ {
    case attr =>
      AttributeSelector(attr)
  } |
  ("[" ~> ident ~ rel ~ (ident|str) <~ "]") ^^ {
    case attr ~ rel ~ value =>
      AttributeSelector(attr + rel + value)
  }

  def TypeParser: Parser[TypeSelector] = ("*" | ident) ^^ {
    case tag =>
      TypeSelector(tag)
  }
}

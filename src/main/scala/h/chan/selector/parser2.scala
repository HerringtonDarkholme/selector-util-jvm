package h.chan.selector

import org.parboiled2._

class CSS(val input: ParserInput) extends Parser {
  def InputLine = rule { List ~ EOI }

  def List = rule {
    oneOrMore(Complex).separatedBy(s_* ~ ',' ~ s_*)
  }

  def Complex = rule { complexStart ~ zeroOrMore(combinator)}

  def complexStart = rule {
    Compound ~> (new ComplexSelector('\u0000', _, null))
  }

  def combinator = rule {
    s_* ~ capture(anyOf(">+~")) ~ s_* ~ Compound ~> {
      (xs: ComplexSelector, c: String, x: CompoundSelector) =>
      new ComplexSelector(c.head, x, xs)
    } |
    s_+ ~ Compound ~> {
      (xs: ComplexSelector, x: CompoundSelector) =>
      new ComplexSelector(' ', x, xs)
    }
  }
  def Compound = rule {
    Tag ~ zeroOrMore(Simple) ~> (CompoundSelector(_, _)) |
    oneOrMore(Simple) ~> (CompoundSelector("*", _))
  }

  def Tag = rule { capture(Ident | '*') }

  def Simple = rule { IDSel | ClsSel | AttrSel | PCSel }

  def IDSel = rule {'#' ~ capture(Ident) ~>
    (AttributeSelector("id", '=', _))
  }

  def ClsSel = rule {'.' ~ capture(Ident) ~>
    (AttributeSelector("class", '=', _))
  }

  def AttrSel = rule { Attr1 | Attr2 }

  def Attr1 = rule {
    '[' ~ capture(Ident) ~ capture(AttrRel) ~ capture(Ident) ~ ']' ~> {
      (a: String, r: String, v: String) => AttributeSelector(a, r.head, v)
  }}
  def Attr2 = rule {
    '[' ~ capture(Ident) ~ ']' ~> (AttributeSelector(_, '\u0000', ""))
  }

  def PCSel = rule {PC1 | PC2}

  def PC1 = rule {':' ~ capture(Ident) ~ '(' ~ capture(Nth) ~ ')' ~> (new NthPC(_, _))}
  def PC2 = rule {':' ~ capture(Ident) ~> (new NthPC(_, ""))}

  def Ident = rule { oneOrMore(CharPredicate.AlphaNum ++ '-' ++ '_') }
  def AttrRel = rule { optional(ch('^') | '~' | '$' | '*' | '|') ~ '='}
  def s_* = rule { zeroOrMore(' ') }
  def s_+ = rule { oneOrMore(' ') }
  def Str = rule {
    ('\'' ~ oneOrMore(!'\'') ~ '\'') |
    ('"' ~ oneOrMore(!'"') ~ '"')
  }
  def Nth = rule { oneOrMore(CharPredicate.AlphaNum ++ '-' ++ '+' ++ 'n') }

}


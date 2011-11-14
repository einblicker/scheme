package scheme
import util.parsing.combinator._

object Parser extends RegexParsers {
  def expr: Parser[Ast] =
    atom | list
  def atom: Parser[Ast] =
    quoted | symbol | string | bool | double
  def quoted: Parser[Ast] =
    "'" ~> expr ^^ {e => SQuote(e)}
  def symbol: Parser[Ast] =
    """[-+*/!?a-zA-Z][-+*/!?a-zA-Z0-9]*""".r ^^ {s => SSymbol(s.toLowerCase)}
  def string: Parser[Ast] =
    """["][^"]*["]""".r  ^^ SString.apply
  def bool: Parser[Ast] =
    """#""" ~> ("t" ^^^ SBool(true) | "f" ^^^ SBool(false))
  def double: Parser[Ast] =
    """[0-9][0-9]*\.?[0-9]*""".r ^^ {s => SDouble(s.toDouble)}
  def list: Parser[Ast] =
    "("~>(rep(expr) ^^ {xs => SList(List(xs: _*))} )<~")"
  def parse(input: String) = parseAll(expr, input).get
}

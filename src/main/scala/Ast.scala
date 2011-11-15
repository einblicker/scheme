package scheme

sealed trait Ast
case class SSymbol(value: String) extends Ast {
  override def toString = value
}
case class SString(value: String) extends Ast {
  override def toString = "\"" + value + "\""
}
case class SBool(value: Boolean) extends Ast {
  override def toString = if (value) "#t" else "#f"
}
case class SDouble(value: Double) extends Ast {
  override def toString = value.toString
}
case class SList(value: List[Ast]) extends Ast {
  override def toString = "(" + value.map(_.toString).mkString(" ") + ")"
}
case class SQuote(value: Ast) extends Ast {
  override def toString = "'" + value
}
case class SProc(params: List[SSymbol], body: Ast, env: Evaluator.Env) extends Ast {
  override def toString = "(lambda "+ "(" + params.mkString(" ") + ")" + " " + body.toString + ")"
}
case class SPrim(name: String, body: List[Ast] => Ast) extends Ast {
  override def toString = "#<primitive function:" + name + ">"
}

object Ast {
  def mkFn(params: List[Ast], body: Ast): Ast =
    SList(List(SSymbol("lambda"), SList(params), body))
  def mkIf(cond: Ast, then: Ast, _else: Ast): Ast =
    SList(List(SSymbol("if"), cond, then, _else))
  def mkApp(fn: Ast, args: List[Ast]): Ast =
    SList(fn :: args)
  def mkSet(name: SSymbol, value: Ast): Ast =
    SList(List(SSymbol("set!"), name, value))
  def mkDefine(name: SSymbol, value: Ast): Ast =
    SList(List(SSymbol("define"), name, value))
}

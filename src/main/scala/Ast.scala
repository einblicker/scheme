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
case class SProc(params: List[SSymbol], body: List[Ast], env: Evaluator.Env) extends Ast {
  override def toString = "(lambda "+ "(" + params.mkString(" ") + ")" + " " + body.mkString(" ") + ")"
}
case class SPrim(name: String, body: List[Ast] => Ast) extends Ast {
  override def toString = "#<primitive function:" + name + ">"
}

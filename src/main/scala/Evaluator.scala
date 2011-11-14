package scheme

object Evaluator {
  import util.control.Breaks._
  import collection.mutable.Map
  
  type Env = List[Map[String, Ast]]

  def lookup(env: Env, name: String): Ast = {
    (for (frame <- env.toStream) yield {
      if (frame.isDefinedAt(name)) {
	Some(frame(name))
      } else None
    }).flatten.head
  }

  def setValue(env: Env, name: String, value: Ast): Unit =
    breakable {
      for (frame <- env) {
	if (frame.isDefinedAt(name)) {
          frame(name) = value
          break
        }
      }
    }

  def eval(prog: Ast, env: Env): Ast = {
    prog match {
      case _: SString | _: SBool | _: SDouble | _: SProc | _: SPrim =>
        prog
      case SQuote(value) =>
	value
      case sym: SSymbol =>
	lookup(env, sym.value)
      case SList(List(SSymbol("set!"), SSymbol(name), value)) =>
	setValue(env, name, eval(value, env))
        SSymbol(name)
      case SList(List(SSymbol("if"), cond, then, _else)) =>
        eval(cond, env) match {
	 case SBool(true) => eval(then, env)
	 case SBool(false) => eval(_else, env)
	}
      case SList(SSymbol("define") :: SList((name: SSymbol) :: lambdaList) :: body) =>
	eval(SList(List(SSymbol("define"), name, SList(SSymbol("lambda") :: SList(lambdaList) :: body))), env)
      case SList(List(SSymbol("define"), SSymbol(name), body)) =>
	env.head += (name -> eval(body, env))
        SSymbol(name)
      case SList(SSymbol("begin") :: xs) =>
	xs.map(eval(_, env)).last
      case SList(SSymbol("lambda") :: SList(lambdaList: List[SSymbol]) :: body) =>
        SProc(lambdaList, body, env)
      case SList(x :: xs) =>
	apply(eval(x, env), xs.map(eval(_, env)))
    }
  }

  def apply(proc: Ast, args: List[Ast]): Ast = {
    proc match {
      case SPrim(_, body) => body(args)
      case SProc(params, body, env) =>
	eval(SList(SSymbol("begin") :: body), Map(params.map(_.value) zip args : _*) :: env)
    }
  }
}
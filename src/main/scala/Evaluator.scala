package scheme

object Evaluator {
  import util.control.Breaks._
  import collection.mutable.Map
  
  type Env = List[Map[String, Ast]]

  def lookup(env: Env, name: String): Ast = {
    val result = for (frame <- env.toStream) yield {
      if (frame.isDefinedAt(name)) {
	Some(frame(name))
      } else None
    }
    result.flatten.head
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
    var isLoop = true
    var currentProg = prog
    var currentEnv  = env
    var result = prog
    def setProgAndEnv(p: Ast, e: Env) = { currentProg = p; currentEnv = e }
    while (isLoop) {
      if (isLoop) isLoop = false
      currentProg match {
        case _: SString | _: SBool | _: SDouble | _: SProc | _: SPrim =>
	  result = prog
        case SQuote(value) =>
          result = value
        case sym: SSymbol =>
          result = lookup(currentEnv, sym.value)
        case SList(List(SSymbol("set!"), SSymbol(name), value)) =>
          setValue(currentEnv, name, eval(value, currentEnv))
          result = SSymbol(name)
        case SList(List(SSymbol("if"), cond, then, _else)) =>
          eval(cond, currentEnv) match {
	   case SBool(true) =>
             setProgAndEnv(then, currentEnv)
	   case SBool(false) =>
             setProgAndEnv(_else, currentEnv)
          }
          isLoop = true
        case SList(List(SSymbol("define"), SSymbol(name), body)) =>
	  Interp.globalEnv(0) += (name -> eval(body, currentEnv))
          result = SSymbol(name)
        case SList(SSymbol("begin") :: xs) =>
	  for((x, i) <- xs.zipWithIndex) {
            if (i == xs.length-1) {
	      setProgAndEnv(x, currentEnv)
	      isLoop = true
	    } else eval(x, currentEnv)
          }
	  isLoop = true
        case SList(List(SSymbol("lambda"), SList(lambdaList: List[SSymbol]), body)) =>
          result = SProc(lambdaList, body, currentEnv)
        case SList(x :: xs) =>
          eval(x, currentEnv) match {
            case SPrim(_, body) => result = body(xs.map(eval(_, currentEnv)))
            case SProc(params, body, env) =>
	      setProgAndEnv(
                body,
                Map(params.map(_.value) zip xs.map(eval(_, currentEnv)) : _*) :: env
              )
	      isLoop = true
	  }
      }
    }
    result
  }
}
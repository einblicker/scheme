package scheme

object Evaluator {
  import collection.mutable.Map
  import Util.gensym
  import Ast.{mkFn, mkApp}
  
  type Env = List[Map[String, Ast]]

  val globalEnv: Env = 
    List(
      Map(
	"eq?" -> SPrim("eq?", {case List(x, y) => SBool(x == y) case _ => sys.error("illegal argument")}),
	"+"   -> SPrim("+",   {case xs: List[SDouble] => SDouble(xs.map(_.value).sum)}),
	"-"   -> SPrim("-",   {case xs: List[SDouble] => SDouble(xs.map(_.value).reduceLeft(_ - _))}),
	"*"   -> SPrim("*",   {case xs: List[SDouble] => SDouble(xs.map(_.value).reduceLeft(_ * _))}),
	"/"   -> SPrim("/",   {case xs: List[SDouble] => SDouble(xs.map(_.value).reduceLeft(_ / _))}),
	"print" -> SPrim("print", {case List(x) => println(x); SList(List()) case Nil => sys.error("too many arguments") }),
	"cons" -> SPrim("cons", {case List(car, SList(cdr)) => SList(car :: cdr) case _ => sys.error("illegal argument")}),
	"car" -> SPrim("car", {case List(SList(car :: _)) => car case _ => sys.error("illegal argument")}),
	"cdr" -> SPrim("cdr", {case List(SList(_ :: cdr)) => SList(cdr) case _ => sys.error("illegal argument")}),
	"exit" -> SPrim("exit", {case _ => sys.exit(0)})
	))

  globalEnv(0) += ("reset/pc" -> {
    val lambda, cont, x = gensym()
    SProc(List(cont, lambda),
	  mkApp(cont,
		List(mkApp(lambda,
			   List(mkFn(List(x), x))))),
	  globalEnv)
  })

  globalEnv(0) += ("call/pc" -> {
    val lambda, cont, cont1, value, x = gensym()
    SProc(List(cont, lambda),
	  mkApp(lambda,
		List(mkFn(List(x), x),
		     mkFn(List(cont1, value),
			  mkApp(cont1,
				List(mkApp(cont, List(value))))))),
	  globalEnv)
  })

  globalEnv(0) += ("call/cc" -> {
    val lambda, cont, dummy, result = gensym()
    SProc(List(cont, lambda),
	  mkApp(lambda,
		List(cont,
		     mkFn(List(dummy, result),
			  mkApp(cont, List(result))))),
	  globalEnv)
  })

  def lookup(env: Env, name: String): Option[Ast] = {
    val result =
      for (frame <- env.toStream) yield {
	if (frame.isDefinedAt(name)) Some(frame(name))
	else None
      }
    result.flatten.headOption
  }

  def setValue(env: Env, name: String, value: Ast) {
    env.find(frame => {
      if (frame.isDefinedAt(name)) {
	frame(name) = value
	true
      } else
	false
    })
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
	  result = currentProg
        case SQuote(value) =>
          result = value
        case sym: SSymbol =>
          result = lookup(currentEnv, sym.value).get
        case SList(List(SSymbol("set!"), SSymbol(name), value)) =>
          setValue(currentEnv, name, eval(value, currentEnv))
          result = SSymbol(name)
        case SList(List(SSymbol("if"), cond, then, _else)) =>
          (eval(cond, currentEnv): @unchecked) match {
	   case SBool(true) =>
             setProgAndEnv(then, currentEnv)
	   case SBool(false) =>
             setProgAndEnv(_else, currentEnv)
          }
          isLoop = true
        case SList(List(SSymbol("define"), SSymbol(name), body)) =>
	  globalEnv(0) += (name -> eval(body, currentEnv))
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
	  (eval(x, currentEnv): @unchecked) match {
            case SPrim(_, body) => result = body(xs.map(eval(_, currentEnv)))
            case SProc(params, body, env) =>
	      setProgAndEnv(
                body,
                Map(params.map(_.value) zip xs.map(eval(_, currentEnv)) : _*) :: env
              )
	      isLoop = true
          }
	case SList(Nil) =>
          result = currentProg
      }
    }
    result
  }
}
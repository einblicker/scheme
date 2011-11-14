package scheme

object CPSConv {

  def isPrimitive(x: Ast): Boolean = {
    x match {
      case SSymbol(name) => Set("*", "-", "/", "+", "print", "eq?").contains(name)
      case _ => false
    }
  }

  val gensym: () => SSymbol = {
    var count = 0
    () => {
      count += 1
      SSymbol("g" + count)
    }
  }
  
  def cps(expr: Ast, cont: Ast => Ast): Ast = {
    
    def cpsSet(name: SSymbol, expr: Ast, cont: Ast => Ast): Ast =
      cps(expr, ast => cont(SList(List(SSymbol("set!"), name, ast))))

    def cpsBegin(body: List[Ast], cont: Ast => Ast): Ast =
      body match {
	case List(x) => cps(x, cont)
	case x :: xs =>
        val g = gensym()
	cpsBegin(xs, ast => {
	 cps(x, ast1 => 
             SList(List(
	       SList(List(SSymbol("lambda"),
			  SList(List(g)),
			  cont(ast))),
	       ast1)))
	 })
	case Nil => cps(SList(Nil), cont)
      }

    def cpsPrimitive(expr: List[Ast], cont: Ast => Ast): Ast =
      cpsArgs(expr.tail, args => cont(SList(expr.head :: args)))

    def cpsArgs(args: List[Ast], cont: List[Ast] => Ast): Ast =
      args match {
	case x :: xs => cps(x, value => cpsArgs(xs, vals => cont(value :: vals)))
	case Nil => cont(Nil)
      }

    def cpsLambda(vars: List[Ast], body: Ast, cont: Ast => Ast): Ast = {
      val cont1 = gensym()
      cont(SList(List(
	SSymbol("lambda"),
	SList(cont1 :: vars),
	cps(body, ast => SList(List(cont1, ast))))))
    }

    def cpsApplication(expr: List[Ast], cont: Ast => Ast): Ast = {
      val value = gensym()
      println(cont(SSymbol("dummy")))
      cpsArgs(expr.tail, ast =>
	SList(expr.head :: SList(List(SSymbol("lambda"),
	                              SList(List(value)),
				      cont(value))) :: ast))
//      SList(List(SSymbol("lambda"),
//		 SList(List(value)),
//		 SList(expr.head :: cont(value) :: ast)))
//	    )
    }
    
    expr match {
      case _: SString | _: SDouble | _: SBool | _: SQuote | _: SSymbol => cont(expr)
      case SList(List(SSymbol("if"), cexpr, texpr, fexpr)) =>
	cps(cexpr, ast => {
          SList(List(SSymbol("if"), ast, cps(texpr, cont), cps(fexpr, cont)))
        })
      case SList(List(SSymbol("define"), name, value)) =>
	cps(value, ast => cont(SList(List(SSymbol("define"), name, ast))))
      case SList(SSymbol("begin") :: rest) =>
	cpsBegin(rest, cont)
      case SList(List(SSymbol("set!"), (name: SSymbol), value)) =>
	cpsSet(name, value, cont)
      case SList(SSymbol("lambda") :: SList(lambdaList) :: rest) =>
	cpsLambda(lambdaList, SList(SSymbol("begin") :: rest), cont)
      case SList(x :: xs) =>
	if (isPrimitive(x)) cpsPrimitive(x :: xs, cont)
        else cpsApplication(x :: xs, cont)
    }
  }
}

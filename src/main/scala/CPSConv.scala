package scheme

object CPSConv {
  import Ast._
  import Util._

  private val primitives = Set("*", "-", "/", "+", "print", "eq?")
  
  def isPrimitive(x: Ast): Boolean =
    x match {
      case SSymbol(name) => primitives.contains(name)
      case _ => false
    }
  
  def cps(expr: Ast, cont: Ast): Ast = {
    
    def cpsAtom(atom: Ast, cont: Ast): Ast =
      mkApp(cont, List(atom))

    def cpsIf(cexpr: Ast, texpr: Ast, fexpr: Ast, cont: Ast): Ast = {
      val g = gensym()
      cps(cexpr, mkFn(List(g), mkIf(g, cps(texpr, cont), cps(fexpr, cont))))
    }

    def cpsSet(name: SSymbol, expr: Ast, cont: Ast): Ast = {
      val g = gensym()
      cps(expr,
	  mkFn(List(g), 
	       mkApp(cont, List(mkSet(name, g)))))
    }
    
    def cpsDefine(name: SSymbol, expr: Ast, cont: Ast): Ast = {
      val g = gensym()
      cps(expr,
	  mkFn(List(g),
	       mkApp(cont, List(mkDefine(name, g)))))
    }

    def cpsBegin(body: List[Ast], cont: Ast): Ast =
      body match {
	case List(x) => cps(x, cont)
	case x :: xs =>
        val g = gensym()
	cps(x, mkFn(List(g), cpsBegin(xs, cont)))
	case Nil => cps(SList(Nil), cont)
      }

    def cpsPrimitive(expr: List[Ast], cont: Ast): Ast = {
      val fn :: args = expr
      val gs = (0 until args.length).map(_ => gensym()).toList
      (args zip gs).reverse.foldRight(mkApp(cont, List(mkApp(fn, gs)))) {
	case ((arg, g), ast) => cps(arg, mkFn(List(g), ast))
      }
    }

    def cpsLambda(vars: List[Ast], body: Ast, cont: Ast): Ast = {
      val g = gensym()
      mkApp(cont, List(mkFn(g :: vars, cps(body, g))))
    }

    def cpsApplication(expr: List[Ast], cont: Ast): Ast =
      (expr: @unchecked) match {
	case List(x) =>
          val g = gensym()
	  cps(x, mkFn(List(g), mkApp(g, List(cont))))
	case List(x, y) =>
	  val g, g1 = gensym()
          cps(x, mkFn(List(g), cps(y, mkFn(List(g1), mkApp(g, List(cont, g1))))))
	case List(x, y, z) =>
          val g, g1, g2 = gensym()
          cps(x, mkFn(List(g), cps(y, mkFn(List(g1), cps(z, mkFn(List(g2), mkApp(g, List(cont, g1, g2))))))))
      }
    
    (expr: @unchecked) match {
      case _: SString | _: SDouble | _: SBool | _: SQuote | _: SSymbol =>
	cpsAtom(expr, cont)
      case SList(List(SSymbol("if"), cexpr, texpr, fexpr)) =>
	cpsIf(cexpr, texpr, fexpr, cont)
      case SList(List(SSymbol("define"), (name: SSymbol), value)) =>
	cpsDefine(name, value, cont)
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

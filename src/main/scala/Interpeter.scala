package scheme

object Interp {
  val globalEnv: Evaluator.Env = List{
    collection.mutable.Map(
      "eq?" -> SPrim("eq?", {case xs: List[SDouble] => SBool(xs(0).value == xs(1).value)}),
      "+" -> SPrim("+", {case xs: List[SDouble] => SDouble(xs.map(_.value).sum)}),
      "-" -> SPrim("-", {case xs: List[SDouble] => SDouble(xs.map(_.value).reduceLeft(_ - _))}),
      "*" -> SPrim("*", {case xs: List[SDouble] => SDouble(xs.map(_.value).reduceLeft(_ * _))}),
      "/" -> SPrim("/", {case xs: List[SDouble] => SDouble(xs.map(_.value).reduceLeft(_ / _))}),
      "print" -> SPrim("print", {case List(x) => println(x); SList(List()) }),
      "call/cc" -> SPrim("call/cc", {
	case List(cont, lambda) =>
   println(cont, lambda)
          Evaluator.eval(
	    SList(List(lambda,
		       cont,
		       SList(List(SSymbol("lambda"),
				  SList(List(SSymbol("_"), SSymbol("result"))),
				  SList(List(cont, SSymbol("result"))))))),
	    globalEnv)
	case x => sys.error(x.toString)
      })
    )
  }

  def apply(input: String) = {
    Evaluator.eval(CPSConv.cps(Parser.parse(input), identity), globalEnv)
  }
}
package scheme

object Interpreter {
  import collection.mutable.Map
  import Ast._
  import Util._
  
  val globalEnv: Evaluator.Env = 
    List(
      Map(
	"eq?" -> SPrim("eq?", {case xs: List[SDouble] => SBool(xs(0).value == xs(1).value)}),
	"+"   -> SPrim("+",   {case xs: List[SDouble] => SDouble(xs.map(_.value).sum)}),
	"-"   -> SPrim("-",   {case xs: List[SDouble] => SDouble(xs.map(_.value).reduceLeft(_ - _))}),
	"*"   -> SPrim("*",   {case xs: List[SDouble] => SDouble(xs.map(_.value).reduceLeft(_ * _))}),
	"/"   -> SPrim("/",   {case xs: List[SDouble] => SDouble(xs.map(_.value).reduceLeft(_ / _))}),
	"print" -> SPrim("print", {case List(x) => println(x); SList(List()) }),
	"call/cc" -> SPrim("call/cc", {
	   case List(cont, lambda) =>
             val g, g1 = gensym()
             Evaluator.eval(
	       mkApp(lambda,
		     List(cont,
			  mkFn(List(g, g1),
			       mkApp(cont, List(g1))))),
	       globalEnv)
         })))

  def apply(input: String) = {
    val g = gensym()
    Evaluator.eval(
      CPSConv.cps(
	Normalizer.normalize(
          Parser.parse(input)),
	mkFn(List(g), g)),
      globalEnv)
  }

  def main(args: Array[String]) = {
    var input: String = Console.readLine("scheme> ")
    Console.flush()
    while (input != ":q") {
      println(apply(input))
      input = Console.readLine("scheme> ")
      Console.flush()
    }
  }
}
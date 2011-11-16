package scheme

object Interpreter {
  import collection.mutable.Map
  import Ast.mkFn
  import Util.gensym

  def apply(input: String) = {
    val g = gensym()
    Evaluator.eval(
      CPSConv.cps(
	Normalizer.normalize(
          Parser.parse(input)),
	mkFn(List(g), g)),
      Evaluator.globalEnv)
  }

  def main(args: Array[String]) = {
    var input: String = Console.readLine("scheme> ")
    Console.flush()
    while (true) {
      try { println(apply(input)) } catch { case e => e.printStackTrace() }
      input = Console.readLine("scheme> ")
      Console.flush()
    }
  }
}
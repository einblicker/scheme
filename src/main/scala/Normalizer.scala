package scheme

object Normalizer {

  def normalize(ast: Ast): Ast =
    ast match {
      case SList(SSymbol("define") :: SList((name: SSymbol) :: lambdaList) :: body) =>
	val lambda = SList(SSymbol("lambda") :: SList(lambdaList) :: body)
	SList(List(SSymbol("define"), name, normalize(lambda)))
      case SList(List(SSymbol("if"), cond, then)) =>
	val ncond = normalize(cond)
        val nthen = normalize(then)
	SList(List(SSymbol("if"), ncond, nthen, SList(List())))
      case SList(SSymbol("lambda") :: (lambdaList: SList) :: body) =>
	val nbody = body.map(normalize)
	SList(List(SSymbol("lambda"), (lambdaList: SList), SList(SSymbol("begin") :: nbody)))
      case _ => ast
    }
  
}
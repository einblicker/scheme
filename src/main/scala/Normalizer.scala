package scheme

object Normalizer {

  def normalize(ast: Ast): Ast =
    ast match {
      case SList(SSymbol("define") :: SList((name: SSymbol) :: lambdaList) :: body) =>
	SList(List(SSymbol("define"), name, SList(SSymbol("lambda") :: SList(lambdaList) :: body)))
      case SList(List(SSymbol("if"), cond, then)) =>
	SList(List(SSymbol("if"), cond, then, SList(List())))
      case SList(SSymbol("lambda") :: (lambdaList: SList) :: body) =>
	SList(List(SSymbol("lambda"), (lambdaList: SList), SList(SSymbol("begin") :: body)))
      case _ => ast
    }
  
}
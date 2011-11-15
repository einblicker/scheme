package scheme

object Util {
  val gensym: () => SSymbol = {
    var count: BigInt = 0
    () => {
      count += 1
      SSymbol("g" + count)
    }
  }
}

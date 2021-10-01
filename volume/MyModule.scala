object MyModule {
  def abs(x: Int): Int = {
    if ( x < 0 ) -x
    else x
  }

  def factorial(n: BigInt): BigInt = {
    def go(n: BigInt, acc: BigInt): BigInt =
      if (n<=0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "Thre absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(x: Int) = {
    val msg = "The factrorial of %d is %d"
    msg.format(x, factorial(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
  }

}
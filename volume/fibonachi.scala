def fib(n: BigInt): BigInt = {

  def go(nth:BigInt, p2: BigInt, p1: BigInt): BigInt = {
    if (nth == 1) return p2
    if (nth == 2) return p1

    go(nth - 1, p1, p1 + p2)
  }
  
  go(n, 0, 1)
}
val rst = fib(202)
println(rst)

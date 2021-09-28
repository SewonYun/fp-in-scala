def factorial(n: BigInt): BigInt = {
  def go(n: BigInt, acc: BigInt): BigInt =
    if (n<=0) acc
    else go(n-1, n*acc)

  go(n, 1)
}

print(factorial(77).toString)
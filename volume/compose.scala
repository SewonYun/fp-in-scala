def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

val doubleTwice = compose((a: Int) => a * a, (b: Int) => b * 2)
println(doubleTwice(3)) // 36
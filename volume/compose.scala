def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

val doubleSquare = compose((a: Int) => a * a, (b: Int) => b * 2)
println(doubleSquare(3)) // 36

val square = (a: Int) => a * a
val double = (b: Int) => b * 2
val stdComposed = square compose double // like  f compose g
val stdAndThen = double andThen square // like  f compose g

println(stdComposed(3)) // 36
println(stdAndThen(4)) // 64

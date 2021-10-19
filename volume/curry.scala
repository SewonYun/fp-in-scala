def curry [A, B, C](f: (A, B) => C) : A => (B => C) = 
  (a: A) => (b: B) => f(a, b)

val plus = curry((a: Int, b: Int) => a + b)
val plus12 = plus(12)
println(plus12(5))


def uncurry [A, B, C](f: A => (B => C)) : (A, B) => C = 
  (a: A, b: B) => f(a)(b)

val pre = uncurry(plus)

println(pre(1, 3))
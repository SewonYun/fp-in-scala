def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
  (b: B) => f(a, b)
}

val plus1 = partial1(1, (a: Int,  b: Int ) => a + b)
println(plus1(3))
def findFirst(ss: Array[Int], key: Int): Int = {
  def loop(n: Int): Int = 
    if (n >= ss.length) -1
    else if (ss(n) == key) n
    else loop(n + 1)

  loop(0)
}

println(findFirst(Array(1,6,23,5,7,3), 22)) // return -1
println(findFirst(Array(1,6,23,5,7,3,23), 23)) // return 2

//parametric polymorphism

def polymorphismFindFirst[A](ss: Array[A], p: A => Boolean): Int = {
  def loop(n: Int): Int = 
    if (n >= ss.length) -1
    else if(p(ss(n))) n
    else loop(n + 1)

  loop(0)
}


println(polymorphismFindFirst(Array(1,6,23,5,7,3), (i: Int) => i == 22)) // return -1
println(polymorphismFindFirst(Array(1,6,23,5,7,3,23), (i: Int) => i == 7)) // return 4
println(polymorphismFindFirst(Array("alba", "respec", "sajangnim"), (s: String) => s == "respec")) // return 1
// println(polymorphismFindFirst(Array("alba", "respec", "sajangnim"), (i: Int) => i == 7)) // error occure
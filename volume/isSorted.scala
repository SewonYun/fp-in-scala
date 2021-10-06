def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): String = {
  
  def go(n: Int): String = 
    if(n + 1 == as.length) "ordered"
    else if(ordered(as(n), as(n+1))) "not ordered"
    else go(n+1)

  go(0)
}

println(isSorted(Array(1, 2, 77, 5), (a: Int, b: Int) => a-b > 0))
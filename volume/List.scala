import java.util._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  /*
  * scala companion object sugar syntax apply def 
  */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => List()
    case Cons(x, xs) => xs
  }

  def setHead[A](v: A, l: List[A]): List[A] = (v, l) match {
    case (Nil, l) => l
    case (v, Nil) => List(v)
    case _ => Cons(v, l)
  }

  def drop[A](l: List[A], n: Int): List[A] = 
    if( n <= 0) l
    else (l, n) match {
      case (Nil, _) => Nil
      case (Cons(x, xs), n) => {
        if(n == 0) Cons(x, xs)
        drop(xs, n-1)
      }
    }

  def dropWhile[A] (l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if(f(x)) => dropWhile(xs, f) 
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil 
    case Cons(x, h) => Cons(x, init(h))
  }

}

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

// print(List.tail(List(1,2,3)))
// print(List.setHead(0, List(1,2,3)))
// print(List.drop(List(1,2,3), 2))
// print(List.dropWhile(List(1,2,3,6,7,8,8,110), (a: Int) => a < 6))
print(List.init(List(1,2,3,6,7,8,8,110,999)))
import java.util._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  /*
   * scala companion object sugar syntax apply def
   */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil         => List()
    case Cons(x, xs) => xs
  }

  def setHead[A](v: A, l: List[A]): List[A] = (v, l) match {
    case (Nil, l) => l
    case (v, Nil) => List(v)
    case _        => Cons(v, l)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      (l, n) match {
        case (Nil, _) => Nil
        case (Cons(x, xs), n) => {
          if (n == 0) Cons(x, xs)
          drop(xs, n - 1)
        }
      }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
    case _                     => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, h)   => Cons(x, init(h))
  }

  //  call stack 이 쌓이는 형태.
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => {
        f(x, foldRight(xs, z)(f))
      }
    }

  // f 의 결과를 바로 인자로 전달 하기 때문에 아마도 꼬리재귀 최적화가 되지 않을까?
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => {
        foldLeft(xs, f(z, x))(f)
      }
    }

  def reverseByFold[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((x: List[A], y: A) => Cons(y, x))

  def leftSum(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def leftProduct(ns: List[Int]) =
    foldLeft(ns, 0)(_ * _)

  def leftLength(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + 1)

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((x, y) => 1 + y)

  def appendByFoldRight[A](l: List[A], a: List[A]): List[A] =
    foldRight(l, a)((x, y) => Cons(x, y))

  def flatConcat[A](l: List[List[A]]): List[A] =
    foldLeft(l, List[A]())(append)

  def plusOneMap(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((x, xs) => Cons(x + 1, xs))

  def echoDoubleNum(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((x, xs) => Cons(x.toString, xs))

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil         => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (f(x)) Cons(x, filter(xs)(f))
      else filter(xs)(f)
    }
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => {
      val b = f(x)
      append(b, flatMap(xs)(f))
    }
  }

  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => {
      if (f(x)) flatMap(xs)(i => List(i))
      else xs
    }
  }

  def sumPairIndex(l: List[Int], p: List[Int]): List[Int] = (l, p) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(l1, l2), Cons(p1, p2)) => Cons(l1 + p1, sumPairIndex(l2, p2))
  }

  def zipWith[A, B, C](l: List[A], p: List[B])(f: (A, B) => C): List[C] =
    (l, p) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(l1, l2), Cons(p1, p2)) => Cons(f(l1, p1), zipWith(l2, p2)(f))
    }

  def last[A](as: List[A]):A = as match {
    case Cons(x, Nil) => x
    case Cons(x, xs)  => last(xs)
    case _ => throw new Exception("no search value");
  }

  def first[A](as: List[A]): A = as match {
    case Cons(x, xs) => x
    case _ => throw new Exception("no search value");
  }

  def hasSubsequence(sup: List[Int], sub: List[Int]):Boolean = {
    if((first(sup) <= first(sub)) && (last(sup) >= last(sub))) {
      true
    } else {
      false
    }
  }

}

// val x = List(1,2,3,4,5) match {
//   case Cons(x, Cons(2, Cons(4, _))) => x
//   case Nil => 42
//   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
//   case Cons(h, t) => h + List.sum(t)
//   case _ => 101
// }

// println(List.tail(List(1,2,3)))
// println(List.setHead(0, List(1,2,3)))
// println(List.drop(List(1,2,3), 2))
// println(List.dropWhile(List(1,2,3,6,7,8,8,110), (a: Int) => a < 6))
// println(List.init(List(1,2,3,6,7,8,8,110,999)))
// println(List.sum2(List(1,2,3,6,7,8,8,110,999)))
// println(List.product2(List(1,2,0,3))) // 당연히 멈추지 않는다 평가가 이루어지기전에 콜스택 끝에 다달아야 하기 때문이다

// println(List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _)))
// println(List.foldLeft(List(1,2,3), 0)( _ + _ ))
// println(List.length(List(1,2,0,3)))
// println(List.leftLength(List(1,2,0,3)))
// println(List.leftSum(List(1,2,0,3)))
// println(List.leftProduct(List(1,2,0,3)))
// println(List.reverseByFold(List(1,2,0,3)))
// println(List.appendByFoldRight(List(1,2,3), List(4)))
// println(List.flatConcat(List(List(1,2,3), List(33,5,6,7))))
// println(List.plusOneMap(List(1,2,3)))
// println(List.echoDoubleNum(List(1,2,3)))
// println(List.map(List(1,2,3))((x: Int) => x * 10))
// println(List.filter(List(1,33,22,3,89))((x: Int) => x > 10))
// println(List.flatMap(List(1,2,3))(i => List(i, i)))
// println(List.filterByFlatMap(List(1,2,3))(x => x > 1))
// println(List.sumPairIndex(List(1,2,3) , List(2,3,4)))
// println(List.zipWith(List(1,2,3) , List(2,3,4))((a, b) => a * b))
// println(List.last(List(1,2,3)))
// println(List.first(List(1,2,3)))
println(List.hasSubsequence(List(1,2,3), List(2,3,4)))

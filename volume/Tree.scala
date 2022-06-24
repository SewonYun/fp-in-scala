sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  val dummy = Branch(
    Leaf(2),
    Branch(Branch(Leaf(12), Branch(Leaf(5), Leaf(67))), Leaf(2))
  )

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)             => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int], acc: Int): Int = t match {
    case Leaf(value)         => value max acc
    case Branch(left, right) => maximum(left, acc) max maximum(right, acc)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)             => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(map(left, f), map(right, f))
  }

  def fold[A, B, C](t: Tree[A], acc: B)(f: (A, B) => C): C = t match {
    case Leaf(v)      => Leaf(f(acc, v))
    case Branch(l, _) => fold(l, acc)
    case Branch(_, r) => fold(r, acc)
  }

}

// println(Tree.size(Tree.dummy))
// println(Tree.maximum(Tree.dummy, 0))
// println(Tree.depth(Tree.dummy))
// println(Tree.map(Tree.dummy, (x: Int) => x.toString))
// println(Tree.map(Tree.dummy, (x: Int) => x*10))

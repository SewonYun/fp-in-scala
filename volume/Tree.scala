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

  def fold[A, B](t: Tree[A], z: B)(branchF: (B, B) => B)(leafF: A => B): B =
    t match {
      case Leaf(value) => leafF(value)
      case Branch(left, right) =>
        branchF(fold(left, z)(branchF)(leafF), fold(right, z)(branchF)(leafF))
    }

}

// println(Tree.size(Tree.dummy))
// println(Tree.maximum(Tree.dummy, 0))
// println(Tree.depth(Tree.dummy))
// println(Tree.map(Tree.dummy, (x: Int) => x.toString))
// println(Tree.map(Tree.dummy, (x: Int) => x*10))
// println(Tree.fold(Tree.dummy, 0)((l: Int, r: Int) => l max r)((x: Int) => x))
// println(Tree.fold(Tree.dummy, 0)((l: Int, r: Int) => l + r)((x: Int) => x))
// println(Tree.fold(Tree.dummy, 0)((l: Int, r: Int) => (l - r))((x: Int) => if(x <= 0) 1 else x))

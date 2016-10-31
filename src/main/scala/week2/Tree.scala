package week2

/**
  * Created by matijav on 31/10/2016.
  */
sealed abstract class Tree[A] {
    val size: Int
}

case class Leaf[A](a: Array[A]) extends Tree[A] {
    override val size = a.size
}

case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
    override val size = l.size + r.size
}

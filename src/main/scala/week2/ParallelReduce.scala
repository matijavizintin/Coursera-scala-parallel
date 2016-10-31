package week2

/**
  * Created by matijav on 31/10/2016.
  */
object ParallelReduce {

    sealed abstract class Tree[A] {}

    case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

    case class Leaf[A](v: A) extends Tree[A]

    def toList[A](t: Tree[A]): List[A] = t match {
        case Leaf(a) => List(a)
        case Node(l, r) => toList(l) ++ toList(r)
    }

    def reduceTreeSeq[A](t: Tree[A], f: (A, A) => A): A = t match {
        case Leaf(v) => v
        case Node(l, r) => f(reduceTreeSeq(l, f), reduceTreeSeq(r, f))
    }

    def reduceTreePar[A](t: Tree[A], f: (A, A) => A): A = t match {
        case Leaf(v) => v
        case Node(l, r) =>
            val (lx, rx) = parallel(reduceTreePar(l, f), reduceTreePar(r, f))
            f(lx, rx)
    }

    def mapTree[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
        case Leaf(a) => Leaf(f(a))
        case Node(l, r) =>
            val (lx, rx) = parallel(mapTree(l, f), mapTree(r, f))
            Node(lx, rx)
    }

    def toList1[A](t: Tree[A]): List[A] = {
        reduceTreePar(mapTree(t, (x: A) => List(x)), (l1: List[A], l2: List[A]) => l1 ++ l2)
    }

    val threshold = 5

    def reduceSeg[A](in: Array[A], l: Int, r: Int, f: (A, A) => A): A = {
        if (r - l < threshold) {
            var res = in(l)
            var i = l + 1

            while (i < r) {
                res = f(res, in(i))
                i += 1
            }
            res
        } else {
            val mid = l + (r - l) / 2
            val (lx, rx) = parallel(reduceSeg(in, l, mid, f), reduceSeg(in, mid, r, f))
            f(lx, rx)
        }
    }

    def reduce[A](in: Array[A], f: (A, A) => A): A = {
        reduceSeg(in, 0, in.length, f)
    }
}

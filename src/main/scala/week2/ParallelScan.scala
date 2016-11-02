package week2

import par.parallel

/**
  * Created by matijav on 31/10/2016.
  */
object ParallelScan {

    sealed abstract class Tree[A]
    case class Leaf[A](a: A) extends Tree[A]
    case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

    sealed abstract class TreeRes[A] {
        val res: A
    }
    case class LeafRes[A](override val res: A) extends TreeRes[A]
    case class NodeRes[A](l: TreeRes[A], override val res: A, r: TreeRes[A]) extends TreeRes[A]

    sealed abstract class TreeResA[A] {
        val res: A
    }
    case class LeafResA[A](from: Int, to: Int, override val res: A) extends TreeResA[A]
    case class NodeResA[A](l: TreeResA[A], override val res: A, r: TreeResA[A]) extends TreeResA[A]

    def scanLeft[A](in: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
        out(0) = a0

        var i = 0
        while (i < in.length) {
            out(i + 1) = f(out(i), in(i))
            i += 1
        }
    }

    def reduceSeq[A1](in: Array[A1], left: Int, right: Int, a0: A1, f: (A1, A1) => A1): A1 = {
        var res = a0
        var i = left
        while(i < right) {
            res = f(res, in(i))
            i += 1
        }
        res
    }

    def scanLeftFunctional[A](in: Array[A], a0: A, f: (A, A) => A, out: Array[A]) {
        def map[A2, B2](in: Array[A2], left: Int, right: Int, f: (Int, A2) => B2, out: Array[B2]): Unit = {
            var i = left
            while (i < right) {
                out(i) = f(i, in(i))
                i += 1
            }
        }



        val fx = (i: Int, a: A) => reduceSeq(in, 0, i, a0, f)
        map(in, 0, in.length, fx, out)
        out(in.length) = f(out(in.length - 1), in(in.length - 1))
    }

    def reduceRes[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
        case Leaf(a) => LeafRes(a)
        case Node(l, r) =>
            val (lx, rx) = (reduceRes(l, f), reduceRes(r, f))
            NodeRes(lx, f(lx.res, rx.res), rx)
    }

    def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
        case Leaf(a) => LeafRes(a)
        case Node(l, r) =>
            val (lx, rx) = parallel(upsweep(l, f), upsweep(r, f))
            NodeRes(lx, f(lx.res, rx.res), rx)
    }

    def downsweep[A](t : TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
        case LeafRes(a) => Leaf(f(a0, a))
        case NodeRes(l, _, r) =>
            val (lx, rx) = parallel(downsweep(l, a0, f), downsweep(r, f(a0, l.res), f))
            Node(lx, rx)
    }

    def scanLeft[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
        val t1 = upsweep(t, f)
        val t2 = downsweep(t1, a0, f)
        prepend(a0, t2)
    }

    def prepend[A](x: A, t: Tree[A]): Tree[A] = t match {
        case Leaf(v) => Node(Leaf(x), Leaf(v))
        case Node(l, r) => Node(prepend(x, l), r)
    }

    val threshold = 3
    def upsweep[A](in: Array[A], from: Int, to: Int, f: (A, A) => A): TreeResA[A] = {
        if (to - from < threshold)
            LeafResA(from, to, reduceSeq(in, from + 1, to, in(from), f))
        else {
            val mid = from + (to - from) / 2
            val (lx, rx) = parallel(upsweep(in, from, mid, f), upsweep(in, mid, to, f))
            NodeResA(lx, f(lx.res, rx.res), rx)
        }
    }

    def downsweep[A](in: Array[A], a0: A, f: (A, A) => A, t: TreeResA[A], out: Array[A]): Unit = t match {
        case LeafResA(from, to, res) => scanLeftSeg(in, from, to, a0, f, out)
        case NodeResA(l, _, r) => parallel(downsweep(in, a0, f, l, out), downsweep(in, f(a0, l.res), f, r, out))
    }

    def scanLeftPar[A](in: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
        val t = upsweep(in, 0, in.length, f)
        downsweep(in, a0, f, t, out)
        out(0) = a0
    }

    def scanLeftSeg[A](in: Array[A], from: Int, to: Int, a0: A, f: (A, A) => A, out: Array[A]): Unit = {
        var a = a0
        var i = from
        while (i < to) {
            a = f(a, in(i))
            i += 1
            out(i) = a
        }
    }
}

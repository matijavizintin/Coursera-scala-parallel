package week4

import scala.annotation.tailrec

/**
  * Created by matijav on 07/11/2016.
  */

sealed trait Conc[+T] {
    def level: Int

    def size: Int

    def left: Conc[T]

    def right: Conc[T]

    def <>[U >: T](that: Conc[U]): Conc[U] = {
        if (this == Empty) that
        else if (that == Empty) this
        else Concs.concat(this, that)
    }
}

case object Empty extends Conc[Nothing] {
    def level = 0

    def size = 0

    override def left: Conc[Nothing] = ???

    override def right: Conc[Nothing] = ???
}

case class Single[T](x: T) extends Conc[T] {
    def level = 0

    def size = 1

    override def left: Conc[T] = ???

    override def right: Conc[T] = ???
}

case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
    val level = 1 + math.max(left.level, right.level)
    val size = left.size + right.size
}

case class Append[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
    val level = 1 + math.max(left.level, right.level)
    val size = left.size + right.size
}

class Chunk[T](val array: Array[T], val size: Int) extends Conc[T] {
    def level = 0

    override def left: Conc[T] = ???

    override def right: Conc[T] = ???
}

object Concs {
    def concat[X](xs: Conc[X], ys: Conc[X]): Conc[X] = {
        val diff = xs.level - ys.level
        if (diff >= -1 && diff <= 1) <>(xs, ys)
        else if (diff < -1) {
            // TODO: impl
            Empty
        } else {
            if (xs.left.level >= xs.right.level) {
                val nr = concat(xs.right, ys)
                <>(xs.left, nr)
            } else {
                val nrr = concat(xs.right.right, ys)
                if (nrr.level == xs.level - 3) {
                    val nl = xs.left
                    val nr = <>(xs.right.left, nrr)
                    <>(nl, nr)
                } else {
                    val nl = <>(xs.left, xs.right.left)
                    val nr = nrr
                    <>(nl, nr)
                }
            }
        }
    }

    def appendLeaf[T](xs: Conc[T], ys: Conc[T]): Conc[T] = xs match {
        case Empty => ys
        case xs: Single[T] => <>(xs, ys)
        case _ <> _ => Append(xs, ys)
        case xs: Append[T] => append(xs, ys)
        case xs: Chunk[T] => <>(xs, ys)
    }

    @tailrec
    private def append[T](xs: Append[T], ys: Conc[T]): Conc[T] = {
        if (xs.right.level > ys.level) Append(xs, ys)
        else {
            val zs = <>(xs.right, ys)
            xs.left match {
                case ws: Append[T] => append(ws, zs)
                case ws if ws.level <= zs.level => ws <> zs
                case ws => Append(ws, zs)
            }
        }
    }
}
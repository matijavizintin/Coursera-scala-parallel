package week2

/**
  * Created by matijav on 31/10/2016.
  */
object ParallelMapping {

    def mapSeq[A, B](xs: List[A], f: A => B): List[B] = xs match {
        case Nil => Nil
        case y :: ys => f(y) :: mapSeq(ys, f)
    }

    def mapASegSeq[A, B](in: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
        var i = left
        while (i < right) {
            out(i) = f(in(i))
            i += 1
        }
    }

    val threshold = 3

    def mapASegPar[A, B](in: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
        if (right - left < threshold) {
            mapASegSeq(in, left, right, f, out)
        } else {
            val mid = left + (right - left) / 2
            parallel(mapASegPar(in, left, mid, f, out), mapASegPar(in, mid, right, f, out))
        }
    }

    def mapTreePar[A:Manifest, B:Manifest](t: Tree[A], f: A => B): Tree[B] = {
        t match {
            case Leaf(a) =>
                val len = a.length
                val b = new Array[B](len)

                for (i <- 0 until len ) {
                    b(i) = f(a(i))
                }
                Leaf(b)
            case Node(l, r) =>
                val (lx, rx) = parallel(mapTreePar(l, f), mapTreePar(r, f))
                Node(lx, rx)
        }
    }
}

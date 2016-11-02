package week3

import par.parallel

import scala.collection.GenSeq

/**
  * Created by matijav on 01/11/2016.
  */
object ParallelData {


    def initializeArray(xs: Array[Int])(v: Int): Unit = {
        for (i <- xs.indices.par) {
            xs(i) = v
        }
    }

    /*def parRender(): Unit = {
        for (i <- xs.indices.par) {
            val (xc, yc) = coordinatesFor(i)
            image(i) = computePixel(xc, yc, maxIterations)
        }
    }*/

    private def computePixel(xc: Double, yc: Double, maxIterations: Int): Int = {
        var i = 0
        var x, y = 0.0
        while (x * x * y * y < 4 && i < maxIterations) {
            val xt = x * x - y * y + xc
            val yt = 2 * x * y + yc
            x = xt
            y = yt
            i += 1
        }
        //        color(i)
        -1
    }

    def countSeq(size: Int): Int = {
        (1 until size).filter(_ % 3 == 0).count(n => n.toString == n.toString.reverse)
    }

    def countPar(size: Int): Int = {
        (1 until size).par.filter(_ % 3 == 0).count(n => n.toString == n.toString.reverse)
    }

    def sumSeq(xs: Array[Int]): Int = {
        xs.foldLeft(0)(_ + _)
    }

    def sumPar(xs: Array[Int], threshold: Int): Int = {
        def sumInt(xs: Array[Int], from: Int, to: Int, threshold: Int): Int = {
            if (to - from < threshold) {
                var acc = 0
                for (x <- from until to) {
                    acc += xs(x)
                }
                acc
            } else {
                val mid = from + (to - from) / 2
                val (x, y) = parallel(sumInt(xs, from, mid, threshold), sumInt(xs, mid, to, threshold))
                x + y
            }
        }

        sumInt(xs, 0, xs.length, threshold)
    }

    def sumPar1(xs: Array[Int]): Int = {
        xs.par.fold(0)(_ + _)
    }

    def seqMax(xs: Array[Int]): Int = {
        xs.max
    }

    def parMax(xs: Array[Int]): Int = {
        xs.par.fold(Int.MinValue)(math.max)
    }

    def largestPalindrome(xs: GenSeq[Int]): Int = {
        xs.aggregate(Int.MinValue)(
            (largest, n) => if (n > largest && n.toString == n.toString.reverse) n else largest,
            math.max
        )
    }
}

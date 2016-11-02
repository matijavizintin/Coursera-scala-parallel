package week3

import org.scalatest.FunSuite
import ParallelData._

/**
  * Created by matijav on 01/11/2016.
  */
class TestParallelData extends FunSuite {

    def init(xs: Array[Int]): Unit = {
        for (x <- xs.indices) {
            xs(x) = x % 1000
        }
    }

    val size = 1000 * 1000 * 100
    test("test seq") {
        val start = System.currentTimeMillis()
        countSeq(size)
        val stop = System.currentTimeMillis()
        println("Seq: " + (stop - start))
    }

    test("test par") {
        val start = System.currentTimeMillis()
        countPar(size)
        val stop = System.currentTimeMillis()
        println("Par: " + (stop - start))
    }

    test("test sum seq") {
        val xs = new Array[Int](size)
        init(xs)

        val start = System.currentTimeMillis()
        sumSeq(xs)
        val stop = System.currentTimeMillis()
        println("Seq sum: " + (stop - start))
    }

    test("test sum par") {
        val xs = new Array[Int](size)
        init(xs)

        val start = System.currentTimeMillis()
        sumPar(xs, 1000)
        val stop = System.currentTimeMillis()
        println("Par sum: " + (stop - start))
    }

    test("test sum par1") {
        val xs = new Array[Int](size)
        init(xs)

        val start = System.currentTimeMillis()
        sumPar1(xs)
        val stop = System.currentTimeMillis()
        println("Par sum 1: " + (stop - start))
    }

    test("test seq max") {
        val xs = new Array[Int](size)
        init(xs)

        val start = System.currentTimeMillis()
        seqMax(xs)
        val stop = System.currentTimeMillis()
        println("Seq Max: " + (stop - start))
    }

    test("test par max") {
        val xs = new Array[Int](size)
        init(xs)

        val start = System.currentTimeMillis()
        parMax(xs)
        val stop = System.currentTimeMillis()
        println("Par max: " + (stop - start))
    }

    test("largest palindrome") {
        val xs = new Array[Int](size)
        init(xs)

        val start = System.currentTimeMillis()
        largestPalindrome(xs)
        val stop = System.currentTimeMillis()
        println("Largest palindrome: " + (stop - start))
    }

    test("largest palindrome par") {
        val xs = new Array[Int](size)
        init(xs)
        val v = Vector(xs:_*)

        val start = System.currentTimeMillis()
        largestPalindrome(v.par)
        val stop = System.currentTimeMillis()
        println("Largest palindrome par: " + (stop - start))
    }
}

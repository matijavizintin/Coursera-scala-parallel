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
        val start = System.currentTimeMillis()
        sumSeq(xs)
        val stop = System.currentTimeMillis()
        println("Seq sum: " + (stop - start))
    }

    test("test sum par") {
        val xs = new Array[Int](size)
        val start = System.currentTimeMillis()
        sumPar(xs, 1000)
        val stop = System.currentTimeMillis()
        println("Par sum: " + (stop - start))
    }

    test("test sum par1") {
        val xs = new Array[Int](size)
        val start = System.currentTimeMillis()
        sumPar1(xs)
        val stop = System.currentTimeMillis()
        println("Par sum 1: " + (stop - start))
    }
}

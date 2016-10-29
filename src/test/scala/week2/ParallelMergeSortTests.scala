package week2

import org.scalatest.FunSuite
import ParallelMergeSort._


/**
  * Created by matijav on 25/10/2016.
  */
class ParallelMergeSortTests extends FunSuite {

    test("quicksort test") {
        val xs = Array(1, 9, 5, 4, 7, 3, 2, 6, 8)
        quickSort(xs, 0, xs.length)
        assert(xs === Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    test("parallel merge sort 1") {
        val xs = Array(1, 9, 5, 4, 7, 3, 2, 6, 8)
        parMergeSort(xs, 1)
        assert(xs === Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    test("parallel merge sort 2") {
        val xs = Array(1, 9, 5, 4, 7, 3, 2, 6, 8)
        parMergeSort(xs, 2)
        assert(xs === Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    test("parallel merge sort 3") {
        val xs = Array(1, 9, 5, 4, 7, 3, 2, 6, 8)
        parMergeSort(xs, 3)
        assert(xs === Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    test("parallel merge sort 15") {
        val xs = Array(1, 9, 5, 4, 7, 3, 2, 6, 8)
        parMergeSort(xs, 15)
        assert(xs === Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    test("parallel merge sort 16") {
        val xs = Array(1, 9, 5, 4, 7, 3, 2, 6, 8)
        parMergeSort(xs, 16)
        assert(xs === Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    test("merge") {
        val xs = Array(1, 3, 5, 7, 2, 4, 8, 9)
        val ys = new Array[Int](8)
        merge(xs, ys, 0, 4, 8)
        assert(ys === Array(1, 2, 3, 4, 5, 7, 8, 9))
    }

    test("merge range") {
        val xs = Array(5, 6, 7, 10, 12, 16, 1, 2, 13, 18, 3, 4)
        val ys = new Array[Int](12)
        merge(xs, ys, 3, 6, 10)

        for (x <- 0 until 3) {
            assert(ys(x) == 0)
        }

        assert(ys.slice(3, 10) === Array(1, 2, 10, 12, 13, 16, 18))

        for (x <- 10 until 12) {
            assert(ys(x) === 0)
        }
    }

    test("sort") {
        val xs = Array(1, 9, 5, 4, 7, 3, 2, 6, 8)
        val ys = new Array[Int](xs.length)
        sort(xs, ys, 0, xs.length, 0, 5)
        assert(ys === Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
    }

    test("sort range") {
        val xs = Array(1, 9, 5, 4, 7, 3, 2, 6, 8)
        val ys = new Array[Int](xs.length)
        sort(xs, ys, 2, 6, 0, 5)
        assert(ys === Array(0, 0, 3, 4, 5, 7, 0, 0, 0))
    }

    test("copy") {
        val xs = new Array[Int](10000)
        initialize(xs)

        val ys = new Array[Int](10000)
        copy(xs, ys, 0, xs.length, 0, 5)
        assert(xs === ys)
    }

    test("copy range") {
        val xs = new Array[Int](100)
        initialize(xs)

        val ys = new Array[Int](100)
        copy(xs, ys, 5, 15, 0, 5)

        for (x <- 0 until 5) {
            assert(ys(x) === 0)
        }
        for (x <- 5 until 15) {
            assert(ys(x) === x % 100)
        }
        for (x <- 15 until 100) {
            assert(ys(x) === 0)
        }
    }
}

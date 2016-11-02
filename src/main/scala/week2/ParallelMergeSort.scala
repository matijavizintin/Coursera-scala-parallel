package week2

import org.scalameter._
import par.parallel


/**
  * Created by matijav on 25/10/2016.
  */
object ParallelMergeSort {

    def quickSort(xs: Array[Int], offset: Int, len: Int): Unit = {
        java.util.Arrays.sort(xs, offset, offset + len)
    }

    @volatile var dummy: AnyRef = null

    def parMergeSort(xs: Array[Int], maxDepth: Int): Unit = {
        val ys = new Array[Int](xs.length)
        dummy = ys

        sort(xs, ys, 0, xs.length, 0, maxDepth)
        if (maxDepth % 2 != 0)
            copy(ys, xs, 0, xs.length, 0, maxDepth)
    }

    def merge(src: Array[Int], dst: Array[Int], from: Int, mid: Int, until: Int): Unit = {
        var left = from
        var right = mid
        var i = from
        while (left < mid && right < until) {
            while (left < mid && src(left) <= src(right)) {
                dst(i) = src(left)
                i += 1
                left += 1
            }
            while (right < until && src(right) <= src(left)) {
                dst(i) = src(right)
                i += 1
                right += 1
            }
        }
        while (left < mid) {
            dst(i) = src(left)
            i += 1
            left += 1
        }
        while (right < until) {
            dst(i) = src(right)
            i += 1
            right += 1
        }
    }

    def sort(xs: Array[Int], ys: Array[Int], from: Int, until: Int, depth: Int, maxDepth: Int): Unit = {
        if (depth == maxDepth) {
            quickSort(xs, from, until - from)
        } else {
            val mid = (from + until) / 2
            parallel(sort(xs, ys, mid, until, depth + 1, maxDepth), sort(xs, ys, from, mid, depth + 1, maxDepth))

            val flip = (maxDepth - depth) % 2 == 0
            val src = if (flip) ys else xs
            val dst = if (flip) xs else ys
            merge(src, dst, from, mid, until)
        }
    }

    def copy(src: Array[Int], target: Array[Int], from: Int, until: Int, depth: Int, maxDepth: Int): Unit = {
        if (depth == maxDepth) {
            Array.copy(src, from, target, from, until - from)
        } else {
            val mid = (from + until) / 2
            parallel(copy(src, target, from, mid, depth + 1, maxDepth), copy(src, target, mid, until, depth + 1, maxDepth))
        }
    }

    val standardConfig = config(
        Key.exec.minWarmupRuns -> 20,
        Key.exec.maxWarmupRuns -> 60,
        Key.exec.benchRuns -> 60,
        Key.verbose -> true
    ) withWarmer (new Warmer.Default)

    def initialize(xs: Array[Int]): Unit = {
        var i = 0
        while (i < xs.length) {
            xs(i) = i % 100
            i += 1
        }
    }

    def main(args: Array[String]): Unit = {
        val length = 100 * 1000 * 1000
        val maxDepth = 7
        val xs = new Array[Int](length)
        val seqtime = standardConfig setUp {
            _ => initialize(xs)
        } measure {
            quickSort(xs, 0, xs.length)
        }
        println(s"sequential sum time: $seqtime ms")

        val partime = standardConfig setUp {
            _ => initialize(xs)
        } measure {
            parMergeSort(xs, maxDepth)
        }
        println(s"fork/join time: $partime ms")
        println(s"speedup: ${seqtime / partime}")
    }
}

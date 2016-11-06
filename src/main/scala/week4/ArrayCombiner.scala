package week4

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import par._
import org.scalameter._

/**
  * Created by matijav on 06/11/2016.
  */
class ArrayCombiner[T <: AnyRef: ClassTag](val parallelism: Int) {
    private var numElems = 0
    private val buffers = new ArrayBuffer[ArrayBuffer[T]]
    buffers += new ArrayBuffer[T]

    def += (x:T) = {    // returns ArrayCombiner[T]
        buffers.last += x
        numElems += 1
        this
    }

    def combine(that: ArrayCombiner[T]) = { // returns ArrayCombiner[T]
        buffers ++= that.buffers
        numElems += that.numElems
        this
    }

    def size = numElems

    def clear() = buffers.clear()

    def copyTo(dst: Array[T], from: Int, to: Int): Unit = {
//        calc initial point in the array of buffers
        var i = from
        var j = 0
        while (i >= buffers(j).length) {
            i -= buffers(j).length
            j += 1
        }

        var k = from
        while (k < to) {
            dst(k) = buffers(j)(i)
            i += 1
            if (i >= buffers(j).length) {
                i = 0
                j += 1
            }
            k += 1
        }
    }

    def result: Array[T] = {
        val array = new Array[T](numElems)
        val step = math.max(1, numElems /  parallelism)
        val starts = (0 until numElems by step) :+ numElems
        val chunks = starts.zip(starts.tail)

        val tasks = for ((from, to) <- chunks) yield task {
            copyTo(array, from, to)
        }
        tasks.foreach(_.join())
        array
    }
}

object ArrayCombiner {
    val standardConfig = config(
        Key.exec.minWarmupRuns -> 20,
        Key.exec.maxWarmupRuns -> 40,
        Key.exec.benchRuns -> 60,
        Key.verbose -> false
    ) withWarmer(new Warmer.Default)

    def main(args: Array[String]): Unit = {
        val size = 1000000

        def run(p: Int): Unit = {
            val taskSupport = new collection.parallel.ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(p))
            val strings = (0 until size).map(_.toString)
            val time = standardConfig measure {
                val parallelized = strings.par
                parallelized.tasksupport = taskSupport
                def newCombiner = new ArrayCombiner(p): ArrayCombiner[String]
                parallelized.aggregate(newCombiner)(_ += _, _ combine _).result
            }
            println(s"p = $p, time = $time ms")
        }

        run(1)
        run(2)
        run(4)
        run(8)
    }
}

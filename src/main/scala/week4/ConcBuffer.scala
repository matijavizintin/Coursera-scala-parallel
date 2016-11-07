package week4

import scala.reflect.ClassTag
import Concs._
import org.scalameter.{Key, Warmer, _}

/**
  * Created by matijav on 07/11/2016.
  */
class ConcBuffer[T: ClassTag](val k: Int, private var conc: Conc[T]) {
    private var chunk: Array[T] = new Array(k)
    private var chunkSize: Int = 0

    final def +=(elem: T): ConcBuffer[T] = {
        if (chunkSize >= k) expand()
        chunk(chunkSize) = elem
        chunkSize += 1
        this
    }

    private def expand(): Unit = {
        conc = appendLeaf(conc, new Chunk(chunk, chunkSize))
        chunk = new Array(k)
        chunkSize = 0
    }

    final def combine(that: ConcBuffer[T]): ConcBuffer[T] = {
        val combinedConc = this.result <> that.result
        new ConcBuffer(k, combinedConc)
    }

    def result: Conc[T] = {
        appendLeaf(conc, new Chunk(chunk, chunkSize))
    }
}

object ConcBuffer {
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
                val newBuffer = new ConcBuffer[String](10, Empty): ConcBuffer[String]
                parallelized.aggregate(newBuffer)((buf, elem) =>  buf += elem, _ combine _).result
            }
            println(s"p = $p, time = $time ms")
        }

        run(1)
        run(2)
        run(4)
        run(8)
    }
}

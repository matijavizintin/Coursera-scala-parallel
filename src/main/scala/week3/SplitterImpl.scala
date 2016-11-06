package week3

import java.util.concurrent.ForkJoinTask
import par.task


/**
  * Created by matijav on 02/11/2016.
  */
class SplitterImpl[T] extends Splitter[T] {

    val threshold = 100

    def fold1(z: T)(f: (T, T) => T): T = {
        if (remaining < threshold) foldLeft(z)(f)
        else {
            val children: Seq[ForkJoinTask[T]] = for (child <- split) yield task {
                child.fold(z)(f)
            }
            children.map(_.join()).foldLeft(z)(f)
        }
    }

    override def split: Seq[Splitter[T]] = ???

    override def remaining: Int = ???

    override def hasNext: Boolean = ???

    override def next(): T = ???
}

trait Splitter[A] extends Iterator[A] {
    def split: Seq[Splitter[A]]

    def remaining: Int
}

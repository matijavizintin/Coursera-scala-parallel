package week4

/**
  * Created by matijav on 07/11/2016.
  */
class ConsList {

    sealed trait List[+T] {
        def head: T
        def tail: List[T]

        def ::[B >: T] (x: B): List[B] =
            new ::(x, this)
    }

    case class ::[T](head: T, tail: List[T]) extends List[T]

    case object Nil extends List[Nothing] {
        def head = sys.error("empty list")
        def tail = sys.error("empty list")
    }

    def filter[T](list: List[T])(predicate: T => Boolean): List[T] = list match {
        case x :: xs if predicate(x) => x :: filter(xs)(predicate)
        case x :: xs => filter(xs)(predicate)
        case Nil => Nil
    }
}

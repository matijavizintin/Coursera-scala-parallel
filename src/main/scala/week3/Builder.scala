package week3

/**
  * Created by matijav on 02/11/2016.
  */
class Builder {

    trait Builder[A, Repr] {
        def +=(elem: A): Builder[A, Repr]

        def result: Repr
    }

    trait Traversable[T] {
        def foreach(f: T => Unit): Unit

        def newBuilder: Builder[T, Traversable[T]]

        def filter(p: T => Boolean): Traversable[T] = {
            val b = newBuilder
            foreach(x => if (p(x)) b += x)
            b.result
        }
    }

    trait Combiner[A, Repr] extends Builder[A, Repr] {
        def combine(that: Combiner[A, Repr]): Combiner[A, Repr]
    }

}

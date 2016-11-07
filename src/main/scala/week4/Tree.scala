package week4

import par._

/**
  * Created by matijav on 07/11/2016.
  */
sealed trait Tree1[+T] {

    def filter[X](t: Tree1[X] ) (p: X => Boolean): Tree1[X] = t match {
        case Node1 (left, right) =>
            val (l, r) = parallel (filter (left) (p), filter (right) (p) )
            Node1 (l, r)
        case Leaf1 (elem) => if (p (elem) ) t else Empty1
        case Empty1 => Empty1
    }
}

case class Node1[T](left: Tree1[T], right: Tree1[T]) extends Tree1[T]

case class Leaf1[T](elem: T) extends Tree1[T]

case object Empty1 extends Tree1[Nothing]



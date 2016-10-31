package week2

import org.scalatest.FunSuite
import ParallelReduce._
import ParallelMergeSort.initialize

/**
  * Created by matijav on 31/10/2016.
  */
class ParallelReduceTest extends FunSuite {
    test("to list test") {
        val t = Node(Node(Leaf(1), Leaf(2)), Leaf(3))
        val l = toList(t)
        assert(l === List(1, 2, 3))
    }

    test("to list function test") {
        val t = Node(Node(Leaf(1), Leaf(2)), Leaf(3))
        val l = toList(t)
        assert(l === List(1, 2, 3))
    }

    test("test reduce tree") {
        val t = Node(Node(Leaf(1), Leaf(2)), Leaf(3))

        val f = (x: Int, y: Int) => x + y
        val res = reduceTreeSeq(t, f)

        assert(res === 6)
    }

    test("test reduce parallel tree") {
        val t = Node(Node(Leaf(1), Leaf(2)), Node(Node(Leaf(10), Node(Leaf(5), Leaf(3))), Leaf(6)))

        val f = (x: Int, y: Int) => x + y
        val res = reduceTreeSeq(t, f)

        assert(res === 27)
    }

    test("test reduce seg") {
        val in = new Array[Int](100)
        initialize(in)

        val f = (a: Int, b: Int) => a + b
        val res = reduce(in, f)

        assert(res === 100 * 99 / 2)
    }
}

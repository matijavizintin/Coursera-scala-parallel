package week2

import org.scalatest.FunSuite
import ParallelMapping._
import ParallelMergeSort._

/**
  * Created by matijav on 31/10/2016.
  */
class ParallelMappingTest extends FunSuite {

    test("map a seg seq") {
        val in = Array(2, 3, 4, 5, 6)
        val out = new Array[Int](5)

        val f = (x: Int) => x * x
        mapASegSeq(in, 1, 3, f, out)

        assert(out(0) === 0)
        assert(out(1) === 9)
        assert(out(2) === 16)
        assert(out(3) === 0)
        assert(out(4) === 0)
    }

    test("map a seg par") {
        val in = new Array[Int](1000)
        val out = new Array[Int](1000)

        initialize(in)

        val f = (x: Int) => x * x
        mapASegPar(in, 1, 999, f, out)

        assert(out(0) === 0)
        assert(out(999) === 0)
        for (x <- 1 until 999) {
            val y = x % 100
            assert(out(x) == y * y)
        }
    }

    test("map a tree par") {
        val t = Node(Node(Leaf[Int](Array(1, 2, 3)), Leaf[Int](Array(4, 5, 6))), Leaf[Int](Array(7, 8, 9)))
        val f = (x: Int) => x * x

        val tx = mapTreePar(t, f)

        var cnt = 0

        def getArray = {
            val a = new Array[Int](3)
            for (i <- 1 to 3) {
                a(i - 1) = (i + cnt * 3) * (i + cnt * 3)
            }
            a
        }

        def evalTree(t: Tree[Int]): Unit = t match {
            case Leaf(a) =>
                assert(a === getArray)
                cnt += 1
            case Node(l, r) =>
                evalTree(l)
                evalTree(r)
        }

        evalTree(tx)
    }
}

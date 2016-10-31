package week2

import org.scalatest.FunSuite
import ParallelScan._

/**
  * Created by matijav on 31/10/2016.
  */
class ParallelScanTest extends FunSuite {

    test("test scan") {
        val in = Array[Int](1, 3, 8)
        val out = new Array[Int](4)
        val f = (agg: Int, x: Int) => agg + x

        scanLeft(in, 100, f, out)

        assert(out === Array[Int](100, 101, 104, 112))
    }

    test("test scan functional") {
        val in = Array[Int](1, 3, 8)
        val out = new Array[Int](4)
        val f = (agg: Int, x: Int) => agg + x

        scanLeftFunctional(in, 100, f, out)

        assert(out === Array[Int](100, 101, 104, 112))
    }

    test("reduce res") {
        val t = Node(Node(Leaf(1), Leaf(2)), Node(Node(Leaf(10), Node(Leaf(5), Leaf(3))), Leaf(6)))
        val tx = NodeRes(NodeRes(LeafRes(1), 3, LeafRes(2)), 27, NodeRes(NodeRes(LeafRes(10), 18, NodeRes(LeafRes(5), 8, LeafRes(3))), 24, LeafRes(6)))

        val f = (x: Int, y: Int) => x + y
        val res = reduceRes(t, f)

        assert(res === tx)
    }

    test("reduce upsweep") {
        val t = Node(Node(Leaf(1), Leaf(2)), Node(Node(Leaf(10), Node(Leaf(5), Leaf(3))), Leaf(6)))
        val tx = NodeRes(NodeRes(LeafRes(1), 3, LeafRes(2)), 27, NodeRes(NodeRes(LeafRes(10), 18, NodeRes(LeafRes(5), 8, LeafRes(3))), 24, LeafRes(6)))

        val f = (x: Int, y: Int) => x + y
        val res = upsweep(t, f)

        assert(res === tx)
    }

    test("reduce downsweep") {
        val t = NodeRes(NodeRes(LeafRes(1), 3, LeafRes(2)), 27, NodeRes(NodeRes(LeafRes(10), 18, NodeRes(LeafRes(5), 8, LeafRes(3))), 24, LeafRes(6)))
        val tx = Node(Node(Leaf(101), Leaf(103)), Node(Node(Leaf(113), Node(Leaf(118), Leaf(121))), Leaf(127)))

        val f = (x: Int, y: Int) => x + y
        val res = downsweep(t, 100, f)

        assert(res === tx)
    }

    test("test scan tree") {
        val t = Node(Node(Leaf(1), Leaf(2)), Node(Node(Leaf(10), Node(Leaf(5), Leaf(3))), Leaf(6)))
        val tx = Node(Node(Node(Leaf(100), Leaf(101)), Leaf(103)), Node(Node(Leaf(113), Node(Leaf(118), Leaf(121))), Leaf(127)))

        val f = (x: Int, y: Int) => x + y
        val res = scanLeft(t, 100, f)

        assert(res === tx)
    }

    test("test scan left parallel") {
        val in = Array[Int](1, 3, 8)
        val out = new Array[Int](4)
        val f = (agg: Int, x: Int) => agg + x

        scanLeftPar(in, 100, f, out)

        assert(out === Array[Int](100, 101, 104, 112))
    }
}

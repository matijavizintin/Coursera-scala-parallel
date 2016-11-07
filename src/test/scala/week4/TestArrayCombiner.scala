package week4

import org.scalatest.FunSuite

/**
  * Created by matijav on 06/11/2016.
  */
class TestArrayCombiner extends FunSuite {

    def init(xs: Array[Int]): Unit = {
        for (x <- xs.indices) {
            xs(x) = x % 1000
        }
    }

    test("test array combiner") {
        val xs = new Array[Int](10000)
        init(xs)

        def newCombiner = new ArrayCombiner(4): ArrayCombiner[Int]
        xs.par.aggregate(newCombiner)((combiner, element) => combiner += element, (c1, c2) => c1 combine c2).result
    }
}

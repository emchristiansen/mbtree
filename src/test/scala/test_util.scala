package mbtree

import org.scalatest.FunSuite

import Util._

class TestUtil extends FunSuite { 
  test("test BreakIntoFolds") { 
    val data = 0 until 15
    // Break into 2 ^ 2 == 4 folds.
    val pairs = BreakIntoFolds(data, 2)

    assert(pairs.size == 4)
    for ((test, train) <- pairs) { 
      // The test and train should be disjoint partitions of |data|.
      assert(test.size + train.size == data.size)
      assert((test ++ train).toSet == data.toSet)
      assert((test.toSet & train.toSet) isEmpty)
    }

    // All the test sets taken together should comprise |data|, and
    // they should not intersect.
    var all_test = Set.empty[Int]
    for ((test, train) <- pairs) { 
      assert((all_test & test.toSet) isEmpty)
      all_test ++= test.toSet
    }
    assert(all_test == data.toSet)
  }
}

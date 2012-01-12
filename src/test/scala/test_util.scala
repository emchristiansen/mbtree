package mbtree

import org.scalatest.FunSuite

import Util._

class TestUtil extends FunSuite { 
  test("test BinarySearch") { 
    def RandomChar = { 
      ('a' to 'z')(Util.random.nextInt(26))
    }

    val random_vals = 100 TimesMap RandomChar
    val sorted = random_vals.sortWith(_ < _)

    val queries = 100 TimesMap RandomChar

    def IsLessThan(x: Char, y: Char): Boolean = x < y

    for (q <- queries) { 
      val index = BinarySearch(q, sorted, IsLessThan)
      for (i <- 0 until index) assert(sorted(i) <= q)
      for (i <- index until sorted.size) assert(sorted(i) >= q)
    }
  }

  test("RandomSubrange should return values in the given range") { 
    val range = -50 until 100
    val r1 = RandomSubrange(range, 10)

    assert(r1.size === 10)
    assert(r1.map(range contains _) === Set(true))

    val r2 = RandomSubrange(range, 100)
    assert(r2.size === 100)
    assert(r2.map(range contains _) === Set(true))
  }

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

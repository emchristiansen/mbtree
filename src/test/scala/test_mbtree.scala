package mbtree

import math._
import org.scalatest.FunSuite

import mbtree.extra._
import Util._

class TestMBTree extends FunSuite {
  val v1 = L2Vector(0, 0, 0)
  val v2 = L2Vector(-1, -1, 1)
  val v3 = L2Vector(2, 2, 1)
  val v4 = L2Vector(1, 1, 1)
  val v5 = L2Vector(1.1, 1.1, 1.1)

  test("spot check MBTreeNN") { 
    val mbtree = new MBTreeNN(Array(v1, v2, v3))

    val (i1, d1) = mbtree.FindNearest(L2Vector(0, 0, .1))
    assert(d1 === .1); assert(i1 === 0)

    val (i2, d2) = mbtree.FindNearest(L2Vector(-1, -2, 4))
    assert(d2 === sqrt(1 + 9)); assert(i2 === 1)
  }

  test("compare BruteNN to MBTreeNN on small dataset") { 
    val data = Array(v1, v2, v3, v4, v5)
    val b = new BruteNN(data)
    val m = new MBTreeNN(data)

    val queries = List(L2Vector(0, 0, .1), v4, L2Vector(1.1, .9, 1.1))
    for (q <- queries) { 
      val (bn, bd) = b.FindNearest(q)
      val (mn, md) = m.FindNearest(q)

      assert(bn === mn); assert(bd === md)
    }
  }

  test("compare BruteNN to MBTree on a bunch of random data") {
    val dimension = 5
    val num_data = 37
    
    val metric_objects = { 
      for (_ <- 0 until num_data) yield { 
	val data = for (_ <- 0 until dimension) yield Util.random.nextGaussian
	L2Vector(data: _*)
      }
    }

    val folds = BreakIntoFolds(metric_objects.toIndexedSeq, 3)

    for ((queries, train) <- folds) { 
      val b = new BruteNN(train)
      val m = new MBTreeNN(train)

      for (q <- queries) { 
	val (bn, bd) = b.FindNearest(q)
	val (mn, md) = m.FindNearest(q)
	
	assert(train(mn).Distance(q) === md); 
	assert(bd === md)	
      }
    }
  }
}

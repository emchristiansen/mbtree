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

  test("compare BruteNN to MBTreeNN with 3 neigbhors on small dataset") { 
    val data = Array(v1, v2, v3, v4, v5)
    val b = new BruteNN(data)
    val m = new MBTreeNN(data)

    val queries = List(L2Vector(0, .1, .1), v4, L2Vector(1.1, .9, 1.1))
    for (q <- queries) { 
      val List((bn0, bd0), (bn1, bd1), (bn2, bd2)) = b.FindNearest(q, 3).toList
      val List((mn0, md0), (mn1, md1), (mn2, md2)) = m.FindNearest(q, 3).toList

      assert(bn0 === mn0); assert(bd0 === md0)
      assert(bn1 === mn1); assert(bd1 === md1)
      assert(bn2 === mn2); assert(bd2 === md2)
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

  test("compare BruteNN to MBTreeNN with 7 NN on a bunch of random data") { 
    val dimension = 5
    val num_data = 37
    val num_neighbors = 7
    
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
  	val bn_with_bds = b.FindNearest(q, num_neighbors)
  	val mn_with_mds = m.FindNearest(q, num_neighbors)
	
  	for (((bn, bd), (mn, md)) <- bn_with_bds.zip(mn_with_mds)) {
  	  assert(train(mn).Distance(q) === md); 
  	  assert(bd === md)	
  	}
      }
    }
  }
}

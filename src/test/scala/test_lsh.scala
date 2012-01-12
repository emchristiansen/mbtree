package mbtree

import org.scalatest.FunSuite

import mbtree.extra._
import Util._

class TestLSH extends FunSuite {
  val v0 = L2Vector(0)
  val v1 = L2Vector(1)
  val v2 = L2Vector(2)
  val v10 = L2Vector(10)
  val v11 = L2Vector(11)

  test("test hasher") { 
    val hasher = new MetricProjectionHasher(List((v0, v2), (v0, v10)))

    assert(hasher.Hash(v0).ToList === List(0, 1))
    assert(hasher.Hash(v2).ToList === List(1))
    assert(hasher.Hash(v11).ToList === List())
  }

  test("test permuter") { 
    val bv0110 = BitVector(List(false, true, true, false))
    val bv1001 = BitVector(List(true, false, false, true))

    assert(bv0110 != bv1001)
    
    val id = List(0, 1, 2, 3)
    val p0 = List(1, 0, 3, 2)

    assert(bv0110 === BitVector.Permute(bv0110, id))
    assert(bv0110 === BitVector.Permute(bv1001, p0))

    val p1 = List(1, 2, 3, 0)
    val bv0011 = BitVector(List(false, false, true, true))
    assert(bv0011 === BitVector.Permute(bv0110, p1))
  }

  test("test the lexicographic ordering of BitVectors") { 
    import CharikarList._

    val bv0110 = BitVector(List(false, true, true, false))
    val bv1001 = BitVector(List(true, false, false, true))
    val bv1000 = BitVector(List(true, false, false, false))

    assert(LexicographicCompare(bv0110, bv0110) === false)
    assert(LexicographicCompare(bv0110, bv1001) === true)
    assert(LexicographicCompare(bv1001, bv0110) === false)
    assert(LexicographicCompare(bv1001, bv1000) === false)
  }

  test("test CharikarList") { 
    val dimension = 8
    val num_points = 20
    val num_queries = 4
    val radius = 4
    val num_permutations = 4

    val bit_vectors = for (_ <- 0 until (num_points + num_queries)) yield { 
      val list = for (_ <- 0 until dimension) yield Util.random.nextBoolean
      BitVector(list.toList)
    }

    assert(bit_vectors.size == num_points + num_queries)

    val (queries, points) = bit_vectors.splitAt(num_queries)

    val permutations = for (i <- 0 until num_permutations) yield { 
      Util.random.shuffle(0 until dimension)
    } toIndexedSeq

    for (query <- queries) { 
      for (permutation <- permutations) { 
	val charikar_list = new CharikarList(points, permutation)

	val neighbors = charikar_list.Nearest(query, radius)

	assert(neighbors.size >= radius)
	assert(neighbors.size <= 2 * radius)
      }
    }
  }

  test("compare BruteNN to LSH on a bunch of random data") {
    val dimension = 10
    val num_data = 1000
    val hash_dimension = 32
    val num_permutations = 4
    
    val metric_objects = { 
      for (_ <- 0 until num_data) yield { 
	val data = for (_ <- 0 until dimension) yield Util.random.nextGaussian
	L2Vector(data: _*)
      }
    }

    val folds = BreakIntoFolds(metric_objects.toIndexedSeq, 3)

    for ((queries, train) <- folds) { 
      val b = new BruteNN(train)
      val l = new MetricLSHNN(train, hash_dimension, num_permutations)

      for (q <- queries) { 
	val (bn, bd) = b.FindNearest(q)
	val (ln, ld) = l.FindNearest(q)

	assert(train(ln).Distance(q) === ld); 
	// Just make sure we're not totally screwing up; make sure we're
	// inside a factor of two of optimal.
	assert(ld <= 2 * bd)	
      }
    }
  }
}

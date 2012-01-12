package mbtree

import collection.immutable.BitSet

case class BitVector private(val bit_set: BitSet, val dimension: Int) extends Metric[BitVector] { 
  if (bit_set.size > 0) require(bit_set.max < dimension)

  def apply(i: Int) = bit_set(i)

  def Distance(that: BitVector): Double = { 
    val differences = for (i <- 0 until dimension) yield { 
      if (bit_set(i) == that.bit_set(i)) 0
      else 1
    }
    differences.sum.toDouble
  }

  def ToList = bit_set.toList.sortWith(_ < _)
}

object BitVector { 
  def apply(bit_list: Seq[Boolean]): BitVector = { 
    var bit_set = BitSet()
    for ((b, i) <- bit_list.zipWithIndex;
         if b == true) { 
      bit_set += i
    }
    BitVector(bit_set, bit_list.size)
  }

  def Permute(bit_vector: BitVector, permutation: Seq[Int]): BitVector = { 
    var permuted = BitSet()
    for ((to, from) <- permutation.zipWithIndex;
	 if (bit_vector.bit_set(from))) { 
      permuted += to
    }
    BitVector(permuted, bit_vector.dimension)
  }
}

class MetricProjectionHasher[T <: Metric[T]](val projection: Seq[(Metric[T], Metric[T])]) { 
  val dimension = projection.size

  def Hash(metric: T): BitVector = { 
    val bit_list = for ((left, right) <- projection) yield { 
      left.Distance(metric) < right.Distance(metric)
    }
    BitVector(bit_list)
  }
}

// An object for returning the most similar bit vector to a given vector,
// according to an similarity implied by the provided permutation.
class CharikarList(val bit_vectors: IndexedSeq[BitVector], val permutation: Seq[Int]) { 
  import CharikarList._

  val permuted = bit_vectors.map(bv => BitVector.Permute(bv, permutation))
  
  val sorted = permuted.zipWithIndex.sortWith((x, y) => LexicographicCompare(x._1, y._1))

  def Nearest(query: BitVector, radius: Int): Seq[Int] = { 
    val permuted = BitVector.Permute(query, permutation)

    // TODO: remove this hacky "-1".
    val nearest = Util.BinarySearch(
	(permuted, -1), 
	sorted, 
	(x: (BitVector, Int), y: (BitVector, Int)) => LexicographicCompare(x._1, y._1))

    val min_index = 0.max(nearest - radius)
    val max_index = sorted.size.min(nearest + radius)
    for (i <- min_index until max_index) yield sorted(i)._2
  }
}

object CharikarList { 
  def LexicographicCompare(x: BitVector, y: BitVector): Boolean = {
    require(x.dimension == y.dimension)

    for (i <- 0 until x.dimension) { 
      if (x(i) < y(i)) return true
      else if (x(i) > y(i)) return false
    }

    return false
  }
}

class CharikarNN[T <: Metric[T]](
    val metrics: IndexedSeq[T], 
    val hasher: MetricProjectionHasher[T],
    val num_permutations: Int) { 
  require(num_permutations > 0)

  val dimension = hasher.dimension

  val bit_vectors = metrics.map(hasher.Hash)

  val permutations = for (i <- 0 until num_permutations) yield { 
    Util.random.shuffle(0 until dimension)
  } toIndexedSeq

  val permuted_lists = for (p <- permutations) yield { 
    new CharikarList(bit_vectors, p)
  }

  def Nearest(query: BitVector, radius: Int): Set[Int] = { 
    val neighbor_lists = for (pl <- permuted_lists) yield pl.Nearest(query, radius)
    neighbor_lists.fold(Seq())(_ ++ _).toSet
  }

  // Compute the Hamming distance between the query BitVector and the bitvector
  // at the index |bit_vectors|.
  def Distance(query: BitVector, index: Int): Double = query.Distance(bit_vectors(index))
}

class MetricLSHNN[T <: Metric[T]](
    val metrics: IndexedSeq[T],
    val dimension: Int,
    val num_permutations: Int) extends NNFinder[T] { 
  require(2 * dimension <= metrics.size)

  val projection_points = Util.RandomSample(metrics, 2 * dimension)
  val projection_pairs = projection_points.take(dimension).zip(projection_points.drop(dimension))

  assert(projection_pairs.size == dimension)

  val hasher = new MetricProjectionHasher(projection_pairs)

  val charikar = new CharikarNN(metrics, hasher, num_permutations)

  def FindNearest(query: T, num: Int): Seq[(Int, Double)] = {
    // Some paramter values that seem reasonable.
    val num_rescore = 10 * num
    val radius = (num_rescore.toDouble / num_permutations.toDouble).ceil.toInt * 10
    FindNearest(query, num, radius, num_rescore)
  }

  // |radius| controls the number of points which will be retrieved
  // from each permutation. |num_rescore| is the number of likely nearest
  // neighbors which will be rescored using the original metric.
  def FindNearest(query: T, num: Int, radius: Int, num_rescore: Int): Seq[(Int, Double)] = { 
    require(num <= num_rescore)

    val hashed_query = hasher.Hash(query)
    val charikar_neighbors = charikar.Nearest(hashed_query, radius).toIndexedSeq
    
    assert(num_rescore <= charikar_neighbors.size)

    // Sort by Hamming distance and take the |num_rescore| nearest.
    val hamming_nearest = charikar_neighbors.sortBy(x => charikar.Distance(hashed_query, x)).take(num_rescore)

    // Sort by the original metric and return the nearest neighbors.
    hamming_nearest.map(i => (i, query.Distance(metrics(i)))).sortBy(_._2).take(num)
  }
}

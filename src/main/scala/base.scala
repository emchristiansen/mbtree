package mbtree

import math._

trait Metric[T] { 
  // This must be a metric in the math sense.
  def Distance(that: T): Double
}

// TODO: change constructor
case class L2Vector(val data: List[Double]) extends Metric[L2Vector] { 
  def Distance(that: L2Vector): Double = {
    val sum_squared = data.zip(that.data).map(x => pow(x._1 - x._2, 2)).sum
    sqrt(sum_squared)
  }
}

object L2Vector { 
  def apply(data: Double*): L2Vector = L2Vector(data.toList)
}

case class L1Vector(val data: List[Double]) extends Metric[L1Vector] { 
  def Distance(that: L1Vector): Double = { 
    data.zip(that.data).map(p => p._1 - p._2).map(abs).sum
  }
}

object L1Vector { 
  def apply(data: Double*): L1Vector = L1Vector(data.toList)
}

// TODO: Come up with a name less lame than "NNFinder"
abstract class NNFinder[T <: Metric[T]] { 
  // Returns the index of the point nearest the query, as well as
  // the distance to that point.
  def FindNearest(query: T): (Int, Double) = { 
    val (index, distance, _) = FindNearestWithCount(query)
    (index, distance)
  }

  // Like findNearest, but also returns the number of metric
  // computations performed.
  // TODO: Get rid of this function, and have metrics keep track
  // of how many times they've been called.
  def FindNearestWithCount(query: T): (Int, Double, Int)
}

// TODO: Consider changing to view bounds "<%" to make these
// methods more general.
class BruteNN[T <: Metric[T]](val data: IndexedSeq[T]) extends NNFinder[T] { 
  def FindNearestWithCount(query: T): (Int, Double, Int) = { 
    val (distance, index) = data.map(x => x.Distance(query)).zipWithIndex.minBy(_._1)
    (index, distance, data.size)
  }
}

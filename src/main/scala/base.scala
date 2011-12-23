package mbtree

import math._

trait Metric[T] { 
  // This must be a metric in the math sense.
  def Distance(that: Metric[T]): Double
}

case class L2Vector(val data: List[Double]) extends Metric[L2Vector] { 
  def Distance(uncast: Metric[L2Vector]): Double = { 
    // TODO: use typesystem to do this intelligently
    val that = uncast.asInstanceOf[L2Vector]
    require(data.length == that.data.length)
    val sum_squared = data.zip(that.data).map(x => pow(x._1 - x._2, 2)).sum
    sqrt(sum_squared)
  }

//  override def toString = data.mkString("[", ",", "]")
}

object L2Vector { 
  def apply(data: Double*): L2Vector = L2Vector(data.toList)

  implicit def toMetric(l2_vector: L2Vector): Metric[L2Vector] = 
      l2_vector.asInstanceOf[Metric[L2Vector]]

  implicit def toL2Vector(metric: Metric[L2Vector]): L2Vector =
      metric.asInstanceOf[L2Vector]
}

// TODO: Come up with a name less lame than "NNFinder"
abstract class NNFinder[T] { 
  // Returns the index of the point nearest the query, as well as
  // the distance to that point.
  def FindNearest(query: Metric[T]): (Int, Double) = { 
    val (index, distance, _) = FindNearestWithCount(query)
    (index, distance)
  }

  // Like findNearest, but also returns the number of metric
  // computations performed.
  // TODO: Get rid of this function, and have metrics keep track
  // of how many times they've been called.
  def FindNearestWithCount(query: Metric[T]): (Int, Double, Int)
}

class BruteNN[T](val data: IndexedSeq[Metric[T]]) extends NNFinder[T] { 
  def FindNearestWithCount(query: Metric[T]): (Int, Double, Int) = { 
    val (distance, index) = data.map(_.Distance(query)).zipWithIndex.minBy(_._1)
    (index, distance, data.size)
  }
}

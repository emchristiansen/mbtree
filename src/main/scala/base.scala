package mbtree

import math._

trait Metric[T] { 
  // This must be a metric in the math sense.
  def Distance(that: T): Double
}

// TODO: Come up with a name less lame than "NNFinder"
abstract class NNFinder[T <: Metric[T]] { 
  // Returns the index of the point nearest the query, as well as
  // the distance to that point.
  def FindNearest(query: T): (Int, Double) = FindNearest(query, 1).head

  // Find |num| nearest neighbors.
  def FindNearest(query: T, num: Int): Seq[(Int, Double)]
}

// TODO: Consider changing to view bounds "<%" to make these
// methods more general.
class BruteNN[T <: Metric[T]](val data: IndexedSeq[T]) extends NNFinder[T] {
  def FindNearest(query: T, num: Int): Seq[(Int, Double)] = {
    val sorted = data.map(x => x.Distance(query)).zipWithIndex.sortBy(_._1)
    sorted.take(num).map(_.swap)
  }
}

// It isn't necessary to import this package to use the library.
package extra {
  case class L2Vector(val data: Double*) extends Metric[L2Vector] { 
    def Distance(that: L2Vector): Double =
      sqrt(data.zip(that.data).map(p => p._1 - p._2).map(x => x * x).sum)
  }

  case class L1Vector(val data: Double*) extends Metric[L1Vector] { 
    def Distance(that: L1Vector): Double =
      data.zip(that.data).map(p => p._1 - p._2).map(abs).sum
  }
}

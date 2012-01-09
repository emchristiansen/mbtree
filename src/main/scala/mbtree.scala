package mbtree

import collection._

import Cluster._
import Util._

class MBTreeNN[T <: Metric[T]](val data: IndexedSeq[T]) extends NNFinder[T] { 
  import MBTree._

  val lookup = { 
    val mutable_map = new mutable.HashMap[T, List[Int]] { 
      override def default(key: T) = List.empty[Int]
    }
    for ((d, i) <- data.zipWithIndex) mutable_map(d) ++= List(i)
    mutable_map.toMap
  }
  
  val tree = RecursiveTwoCenters(lookup.keys.toSet)

  def FindNearest(query: T, num: Int): Seq[(Int, Double)] = { 
    val neighbor_with_distances = FindExactNNPruning(query, tree, num)

    // Just take the first index.
    val indices = for ((n, d) <- neighbor_with_distances) yield lookup(n).head
    val distances = neighbor_with_distances.map(_._2)
    indices.zip(distances)
  }
}

// TODO: consider renaming this file and object "search",
// and move the MBTreeNN class to another file, along with BruteNN.
object MBTree {
  // Invariant: The guess distance is always at least as good as the distance
  // to the root of the tree.
  def FindExactNNPruningHelper[T <: Metric[T]](
      query: T, 
      subtree: Tree[Ball[T]], 
      guess_with_distances: BoundedPriorityQueue[(T, Double)]): BoundedPriorityQueue[(T, Double)] = {
    // Update the best neighbor by considering the root of the tree.
    // TODO: consider making this functional (no var)
    val children_distances = subtree.children.map(_.data.center.Distance(query))
    val children_with_distances = subtree.children.zip(children_distances).sortBy(_._2)

    // Mutability. Ugh.
    var best_with_distances = guess_with_distances

    // The distance of the farthest neighbor, or Double.MaxValue if the bounded
    // priority queue is not yet full.
    def worst_distance: Double = { 
      if (!best_with_distances.AtCapacity) Double.MaxValue
      else best_with_distances.data.head._2
    }

    // Examine the children, if any, for a closer neighbor.
    for ((c, d) <- children_with_distances;
         if d - c.data.radius < worst_distance) {
      if (d < worst_distance) {
	best_with_distances += (c.data.center, d)
      }

      best_with_distances = 
	  FindExactNNPruningHelper(query, c, best_with_distances)

      // // If we find better values in the child, update.
      // if (child_best_distance < best_distance) {
      // 	best = child_best
      // 	best_distance = child_best_distance
      // }
    }
      
    best_with_distances
  }

  def FindExactNNPruning[T <: Metric[T]](
      query: T, tree: Tree[Ball[T]], num: Int): Seq[(T, Double)] = {
    // Just use the root of the tree as the guess.
    val guess = tree.data.center
    val guess_distance = query.Distance(guess)
    val guess_with_distances = new BoundedPriorityQueue[(T, Double)](num)(Ordering.by(_._2))
    guess_with_distances += (guess, guess_distance)
    FindExactNNPruningHelper(query, tree, guess_with_distances).AsSortedList
  }
}

package mbtree

import collection._

import Cluster._

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

  def FindNearest(query: T): (Int, Double) = { 
    val (nearest, distance) = FindExactNNPruning(query, tree)

    // Just take the first index.
    val index = lookup(nearest).head
    (index, distance)
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
      guess: T,
      guess_distance: Double): (T, Double) = {
    // Update the best neighbor by considering the root of the tree.
    // TODO: consider making this functional (no var)
    val children_distances = subtree.children.map(_.data.center.Distance(query))
    val children_with_distances = subtree.children.zip(children_distances).sortBy(_._2)

    // Examine the children, if any, for a closer neighbor.
    var best = guess
    var best_distance = guess_distance
    for ((c, d) <- children_with_distances;
         if d - c.data.radius < best_distance) {
      if (d < best_distance) { 
	best = c.data.center
	best_distance = d
      }

      val (child_best, child_best_distance) = 
	  FindExactNNPruningHelper(query, c, best, best_distance)

      // If we find better values in the child, update.
      if (child_best_distance < best_distance) {
	best = child_best
	best_distance = child_best_distance
      }
    }
      
    (best, best_distance)
  }

  def FindExactNNPruning[T <: Metric[T]](
      query: T, tree: Tree[Ball[T]]): (T, Double) = {
    // Just use the root of the tree as the guess.
    val guess = tree.data.center
    FindExactNNPruningHelper(query, tree, guess, query.Distance(guess))
  }
}

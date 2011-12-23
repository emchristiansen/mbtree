package mbtree

import collection._

import Cluster._

class MBTreeNN[T](val data: IndexedSeq[Metric[T]]) extends NNFinder[T] { 
  import MBTree._

  val lookup = { 
    val mutable_map = new mutable.HashMap[Metric[T], List[Int]] { 
      override def default(key: Metric[T]) = List.empty[Int]
    }
    for ((d, i) <- data.zipWithIndex) mutable_map(d) ++= List(i)
    mutable_map.toMap
  }
  
  val tree = RecursiveTwoCenters(lookup.keys.toSet)

  def FindNearestWithCount(query: Metric[T]): (Int, Double, Int) = { 
    val (nearest, distance, count) = FindExactNNPruning(query, tree)

    // Just take the first index.
    val index = lookup(nearest).head
    (index, distance, count)
  }
}

// TODO: consider renaming this file and object "search",
// and move the MBTreeNN class to another file, along with BruteNN.
object MBTree {
  def FindExactNNPruningHelper[T](
      query: Metric[T], 
      subtree: Tree[Ball[T]], 
      guess: Metric[T], 
      num_metric_calls: Int): (Metric[T], Double, Int) = {
    // Update the best neighbor by considering the root of the tree.
    // TODO: consider making this functional (no var)
    // TODO: avoid recalculating distances
    var (best, best_distance) = { 
      val guess_distance = query.Distance(guess)
      val root_distance = query.Distance(subtree.data.center)
      if (guess_distance < root_distance) (guess, guess_distance)
      else (subtree.data.center, root_distance)
    }

    // Examine the children, if any, for a closer neighbor.
    var num_metric_calls_here = 1
    for (c <- subtree.children;
         if query.Distance(c.data.center) - c.data.radius < best_distance) { 
      val (child_best, child_best_distance, metric_calls) = 
	  FindExactNNPruningHelper(query, c, best, 0)
      num_metric_calls_here += metric_calls

      // If we find better values in the child, update.
      if (child_best_distance < best_distance) {
	best = child_best
	best_distance = child_best_distance
      }
    }
      
    (best, best_distance, num_metric_calls + num_metric_calls_here)
  }

  def FindExactNNPruning[T](
      query: Metric[T], tree: Tree[Ball[T]]): (Metric[T], Double, Int) = {
    // Just use the root of the tree as the guess.
    val guess = tree.data.center
    FindExactNNPruningHelper(query, tree, guess, 0)
  }
}

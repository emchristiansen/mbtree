package mbtree

import Cluster._

class MBTreeNN[T](val data: IndexedSeq[Metric[T]]) extends NNFinder[T] { 
  import MBTree._
  
  val multi_set = MultiSet(data: _*)
  val tree = RecursiveTwoCenters(multi_set)

  def FindNearestWithCount(query: Metric[T]): (Int, Double, Int) = { 
    val (nearest, distance, count) = FindExactNNPruning(query, tree)

    // TODO: Refactor to make it natural to return the index (rather
    // than having to do this stupid lookup which is linear cost).
    val index = data.indexOf(nearest)
    (index, distance, count)
  }
}

// TODO: consider renaming this file and object "search",
// and move the MBTreeNN class to another file, along with BruteNN.
object MBTree {
  def FindExactNNPruning[T](
      query: Metric[T], tree: Tree[Ball[T]]): (Metric[T], Double, Int) = { 
    // TODO: make this accurate
    var num_metric_calls = 0

    def Helper[T](
      query: Metric[T], tree: Tree[Ball[T]], guess: Metric[T]): 
    (Metric[T], Double) = {
      // Update the best neighbor by considering the root of the tree.
      // TODO: consider making this functional (no var)
      var (best, best_distance) = { 
	val guess_distance = query.Distance(guess)
	val root_distance = query.Distance(tree.data.center)
	if (guess_distance < root_distance) (guess, guess_distance)
	else (tree.data.center, root_distance)
      }

      // TODO: currently just looking at calls to leafs
      //if (tree.children.isEmpty) num_metric_calls += 1
      num_metric_calls += 1

      // Examine the children, if any, for a closer neighbor.
      for (c <- tree.children;
           if query.Distance(c.data.center) - c.data.radius < best_distance) { 
	     val (child_best, child_best_distance) = 
	       Helper(query, c, best)

	     // If we find better values in the child, update.
	     if (child_best_distance < best_distance) { 
	       best = child_best
	       best_distance = child_best_distance
	     }
	   }
      
      (best, best_distance)
    }

    // Just use the root of the tree as the guess.
    val guess = tree.data.center
    val (nearest, distance) = Helper(query, tree, guess)
    (nearest, distance, num_metric_calls)
  }

  // def FindNearestNeighborAndDistance[T](
  //     query: Metric[T], 
  //     tree: Tree[Ball[T]]): (Metric[T], Double) = { 
  //   def Helper(nearest: Metric[T], 
  // 	       distance: Double, 
  // 	       trees: List[Tree[Ball[T]]]): (Metric[T], Double) = { 
  //     trees match { 
  // 	case Nil => (nearest, distance)
  // 	case tree :: tail => { 
  // 	  val centroid_distance = query.Distance(tree.data.center)
  // 	  if (centroid_distance - tree.data.radius > distance) { 
  // 	    // By the triangle inequality we can skip this tree.
  // 	    Helper(nearest, distance, tail)
  // 	  } else {
  // 	    // Search down this path.
  // 	    val (tree_nearest, tree_distance) = 
  // 		FindNearestNeighborAndDistance(query, tree)
  // 	    val (n, d) = 
  // 	        if (distance < tree_distance) (nearest, distance) 
  // 		else (tree_nearest, tree_distance)
  // 	    Helper(n, d, tail)
  // 	  }
  // 	}
  //     }
  //   }

  //   val children_with_distances = tree.children.map(t => (t, query.Distance(t.data.center)))
  //   val sorted_children = children_with_distances.sortWith(_._2 < _._2).map(_._1)
  //   Helper(tree.data.center, query.Distance(tree.data.center), sorted_children)
  // }
}

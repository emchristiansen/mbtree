package mbtree

import Cluster._

object MBTree {
  // Greedily follows the tree to a leaf.
  def FindApproximateNNGreedy[T](
      query: Metric[T], tree: Tree[Ball[T]]): (Metric[T], Double) = { 
    if (tree.children.isEmpty) 
      (tree.data.center, query.Distance(tree.data.center))
    else { 
      val nearest_child = tree.children.minBy(_.center.Distance(query) < _.center.Distance(query))
      FindApproximateNNGreedy(query, nearest_child)
    }
  }

  def FindExactNNPruningWithGuess[T](
      query: Metric[T], tree: Tree[Ball[T]], best: Metric[T]): 
      (Metric[T], Double) = { 
    
  }

  def FindNearestNeighborAndDistance[T](
      query: Metric[T], 
      cluster_tree: Tree[Cluster[T]]): (Metric[T], Double) = { 
    def Helper(nearest: Metric[T], 
	       distance: Double, 
	       trees: List[Tree[Cluster[T]]]): (Metric[T], Double) = { 
      trees match { 
	case Nil => (nearest, distance)
	case tree :: tail => { 
	  val centroid_distance = query.Distance(tree.data.centroid)
	  if (centroid_distance - tree.data.error > distance) { 
	    // By the triangle inequality we can skip this tree.
	    Helper(nearest, distance, tail)
	  } else { 
	    val (tree_nearest, tree_distance) = 
		FindNearestNeighborAndDistance(query, tree)
	    val (n, d) = 
	        if (distance < tree_distance) (nearest, distance) 
		else (tree_nearest, tree_distance)
	    Helper(n, d, tail)
	  }
	}
      }
    }

    val children_with_distances = cluster_tree.children.map(t => (t, query.Distance(t.data.centroid)))
    val sorted_children = children_with_distances.sortWith(_._2 < _._2).map(_._1)
    Helper(cluster_tree.data.centroid, query.Distance(cluster_tree.data.centroid), sorted_children)
  }
}

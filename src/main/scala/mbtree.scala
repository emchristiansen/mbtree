package mbtree

// TODO: It isn't necessary to always keep the data members of the cluster.
case class Cluster[T](val centroid: Metric[T], val members: Seq[Metric[T]], val error: Double) { 
  if (!members.contains(centroid)) { 
    println("WTF??")
    println(centroid)
    println(members)
    println("/WTF??")
  }
  require(members.contains(centroid))
  require(error >= 0)

  override def toString = {
    val error_string = if (error == Double.MaxValue) "INF" else "%.8f".format(error)
    "centroid: %s, error: %s".format(centroid.toString, error_string)
  }
}

case class Tree[T](val data: T, children: List[Tree[T]]) { 
  def toDotExpressions: List[String] = { 
    val syntax = "\"%s\" -> \"%s\";"
    val expressions = for (c <- children) yield syntax.format(data.toString, c.data.toString)
    expressions ++ children.flatMap(_.toDotExpressions)
  }

  def toDot: String = { 
    "digraph Tree {\n" + toDotExpressions.mkString("\n") + "\n}"
  }
}

object MBTree {
  def TwoMeansAssignToCentroids[T](
      centroid_0: Metric[T], 
      centroid_1: Metric[T],
      metric_objects: Seq[Metric[T]]): 
      (Seq[Metric[T]], Seq[Metric[T]]) = { 
    metric_objects.partition(x => 
        x.Distance(centroid_0) < x.Distance(centroid_1))
  }

  def FindCentroidWithError[T](metric_objects: Seq[Metric[T]]): (Metric[T], Double) = { 
    require(metric_objects.size > 0)

    // TODO: Make the error function a parameter
    def CumulativeError(metric_object: Metric[T]): Double = 
        metric_objects.map(x => x.Distance(metric_object)).sum

    val centroid_pair_errors = metric_objects.map(x => (x, CumulativeError(x)))
    centroid_pair_errors.sortWith(_._2 < _._2).head
  }

  def MakeCluster[T](metric_objects: Seq[Metric[T]]): Cluster[T] = { 
    require(metric_objects.size > 0)

    val (centroid, error) = FindCentroidWithError(metric_objects)
    Cluster(centroid, metric_objects, error)
  }

  def TwoMeansSeeded[T](centroid_0: Metric[T], 
			centroid_1: Metric[T], 
			metric_objects: Seq[Metric[T]]): (Cluster[T], Cluster[T]) = { 
    require(metric_objects.contains(centroid_0))
    require(metric_objects.contains(centroid_1))

    def Helper(cluster_0: Cluster[T], cluster_1: Cluster[T]): (Cluster[T], Cluster[T]) = { 
      val (members_0, members_1) = 
	  TwoMeansAssignToCentroids(cluster_0.centroid, 
				    cluster_1.centroid, 
				    cluster_0.members ++ cluster_1.members)
      val new_cluster_0 = MakeCluster(members_0)
      val new_cluster_1 = MakeCluster(members_1)
      if (new_cluster_0 != cluster_0 || new_cluster_1 != cluster_1)
	Helper(new_cluster_0, new_cluster_1)
      else (new_cluster_0, new_cluster_1)
    }

    // Initialize with everything in |cluster_0| except the
    // centroid of |cluster_1|. We have to be careful, as there may be
    // duplicate points.
    val members_0 = { 
      val index = metric_objects.indexWhere(_ == centroid_1)
      metric_objects.take(index) ++ metric_objects.drop(index + 1)
    }
    val cluster_0 = Cluster(centroid_0, members_0, Double.MaxValue)
    val cluster_1 = Cluster(centroid_1, List(centroid_1), Double.MaxValue)
    Helper(cluster_0, cluster_1)
  }

  def TwoMeans[T](metric_objects: Seq[Metric[T]]): (Cluster[T], Cluster[T]) = {
    val shuffled = Util.random.shuffle(metric_objects)
    val centroid_0 :: centroid_1 :: _ = shuffled
    TwoMeansSeeded(centroid_0, centroid_1, metric_objects)
  }

  def RecursiveTwoMeansHelper[T](cluster: Cluster[T]): Tree[Cluster[T]] = {
    require(cluster.members.size > 0)

    if (cluster.members.size == 1) Tree(cluster, List())
    else { 
      val (cluster_0, cluster_1) = TwoMeans(cluster.members)
      val tree_0 = RecursiveTwoMeansHelper(cluster_0)
      val tree_1 = RecursiveTwoMeansHelper(cluster_1)
      Tree(cluster, List(tree_0, tree_1))
    }
  }

  def RecursiveTwoMeans[T](metric_objects: Seq[Metric[T]]): Tree[Cluster[T]] = { 
    val (centroid, error) = FindCentroidWithError(metric_objects)
    val cluster = Cluster(centroid, metric_objects, error)
    RecursiveTwoMeansHelper(cluster)
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

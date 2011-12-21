package mbtree

// A ball in a metric space.
case class Ball[T](val center: Metric[T], val radius: Double)

// A DataBall is a set of data along with a ball that contains the data.
// The programmer must ensure the provided data is contained in the
// provided ball.
// TODO: Consider getting rid of this class.
case class DataBall[T](val ball: Ball[T], val data: Bag[Metric[T]]) { 
  def center = ball.center

  def radius = ball.radius

  def size = data.sizeNotDistinct

  override def toString = { 
    val format = "DataBall:\nCenter: %s\nRadius: %.8f\nNum Elements: %d\n"
    format.format(center.toString, radius, size)
  }
}

object Cluster { 
  def TwoCentersAssignToCenters[T](
      center_0: Metric[T], 
      center_1: Metric[T], 
      metric_objects: Bag[Metric[T]]): 
      (Bag[Metric[T]], Bag[Metric[T]]) = {     
    metric_objects.partition {case(key, _) => key.Distance(center_0) <= key.Distance(center_1))}
  }

  def MinCoveringBall[T](
      metric_objects: Bag[Metric[T]]): Ball[T] = { 
    require(metric_objects.size > 0)

    // TODO: Make the error function a parameter
    def MaxError(metric_object: Metric[T]): Double = 
        metric_objects.map {case (key, _) => key.Distance(metric_object)} max

    val centroid_pair_errors = metric_objects.map(x => (x, MaxError(x._1)))
    val (center, radius) = centroid_pair_errors.minBy(_._2)
    // TODO: Will need to change this when stop storing data redundantly.
    Ball(center, radius)
  }

  // Cluster data using two centers method, given initial
  // guesses of the centroids.
  def TwoCentersSeeded[T](
      centroid_0: Metric[T], 
      centroid_1: Metric[T], 
      metric_objects: Bag[Metric[T]]): 
      (DataBall[T], DataBall[T]) = { 
    // TODO: This check is expensive and probably unnecessary.
    require(metric_objects.Contains(centroid_0))
    require(metric_objects.Contains(centroid_1))

    def Helper(
	data_ball_0: DataBall[T], 
	data_ball_1: DataBall[T]): 
	(DataBall[T], DataBall[T]) = { 
      val (new_data_0, new_data_1) = 
	  TwoCentersAssignToCenters(data_ball_0.center, 
				   data_ball_1.center, 
				   data_ball_0.data ++ data_ball_1.data)
      val new_ball_0 = MinCoveringBall(new_data_0)
      val new_data_ball_0 = DataBall(new_ball_0, new_data_0)
      val new_ball_1 = MinCoveringBall(new_data_1)
      val new_data_ball_1 = DataBall(new_ball_1, new_data_1)
      
      // Recursively call this function until convergence.
//      if (new_data_ball_0 != data_ball_0 || new_data_ball_1 != data_ball_1)
      if (new_ball_0 != data_ball_0.ball || new_ball_1 != data_ball_1.ball)
	Helper(new_data_ball_0, new_data_ball_1)
      else (new_data_ball_0, new_data_ball_1)
    }

    // Initialize with everything in |cluster_0| except the
    // centroid of |cluster_1|. We have to be careful, as there may be
    // duplicate points.
    // val members_0 = { 
    //   val index = metric_objects.indexWhere(_ == centroid_1)
    //   metric_objects.take(index) ++ metric_objects.drop(index + 1)
    // }
    val members_0 = { 
      val ret = metric_objects.Clone
      ret.Remove(centroid_1)
      ret
    }
    val members_1 = { 
      val ret = new Bag[Metric[T]]
      ret.Add(centroid_1)
      ret
    }
    val data_ball_0 = DataBall(Ball(centroid_0, Double.MaxValue), members_0)
    val data_ball_1 = DataBall(Ball(centroid_1, Double.MaxValue), members_1)
    Helper(data_ball_0, data_ball_1)
  }

  // Two centers clustering, selecting random elements 
  // to be the initial centroids.
  def TwoCenters[T](
	metric_objects: Bag[Metric[T]]): (DataBall[T], DataBall[T]) = {
    val shuffled = Util.random.shuffle(metric_objects.ElementsNoCount)
    val centroid_0 :: centroid_1 :: _ = shuffled.toList
    TwoCentersSeeded(centroid_0, centroid_1, metric_objects)
  }

  def RecursiveTwoCentersHelper[T](data_ball: DataBall[T]): Tree[Ball[T]] = {
    require(data_ball.size > 0)

    if (data_ball.size == 1) { 
      assert(data_ball.radius == 0)
      Tree(data_ball.ball, List())
    }
    else { 
      val (data_ball_0, data_ball_1) = TwoCenters(data_ball.data)
      val tree_0 = RecursiveTwoCentersHelper(data_ball_0)
      val tree_1 = RecursiveTwoCentersHelper(data_ball_1)
      Tree(data_ball.ball, List(tree_0, tree_1))
    }
  }

  // Recursively clusters the data, producing a tree of Balls. The leafs of
  // the tree are Balls of radius 0, corresponding to individual elements.
  def RecursiveTwoCenters[T](metric_objects: Bag[Metric[T]]): Tree[Ball[T]] = { 
    val ball = MinCoveringBall(metric_objects)
    val data_ball = DataBall(ball, metric_objects)
    RecursiveTwoCentersHelper(data_ball)
  }
}

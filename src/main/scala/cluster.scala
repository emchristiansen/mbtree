package mbtree

// A ball in a metric space.
case class Ball[T](val center: T, val radius: Double)

// A DataBall is a set of data along with a ball that contains the data.
// The programmer must ensure the provided data is contained in the
// provided ball.
// TODO: Use a private constructor to guarantee the above property.
case class DataBall[T](val ball: Ball[T], val data: Set[T]) { 
  def center = ball.center

  def radius = ball.radius

  def size = data.size

  override def toString = { 
    val format = "DataBall:\nCenter: %s\nRadius: %.8f\nNum Elements: %d\n"
    format.format(center.toString, radius, size)
  }
}

object Cluster { 
  def TwoCentersAssignToCenters[T <: Metric[T]](
      center_0: T, 
      center_1: T, 
      metrics: Set[T]): 
      (Set[T], Set[T]) = {     
    metrics.partition(x => x.Distance(center_0) <= x.Distance(center_1))
  }

  def MinCoveringBall[T <: Metric[T]](
      metrics: Set[T]): Ball[T] = { 
    require(metrics.size > 0)

    // TODO: Make the error function a parameter
    def MaxError(metric: T): Double = 
        metrics.map(x => x.Distance(metric)).max

    val centroid_pair_errors = metrics.map(x => (x, MaxError(x)))
    val (center, radius) = centroid_pair_errors.minBy(_._2)
    // TODO: Will need to change this when stop storing data redundantly.
    Ball(center, radius)
  }

  def TwoCentersSeededHelper[T <: Metric[T]](
      data_ball_0: DataBall[T], 
      data_ball_1: DataBall[T]): (DataBall[T], DataBall[T]) = {
    val (new_data_0, new_data_1) = 
	TwoCentersAssignToCenters(data_ball_0.center, 
 				  data_ball_1.center, 
				  data_ball_0.data ++ data_ball_1.data)
    val new_ball_0 = MinCoveringBall(new_data_0)
    val new_data_ball_0 = DataBall(new_ball_0, new_data_0)
    val new_ball_1 = MinCoveringBall(new_data_1)
    val new_data_ball_1 = DataBall(new_ball_1, new_data_1)

    // Recursively call this function until convergence.
    if (new_ball_0 != data_ball_0.ball || new_ball_1 != data_ball_1.ball)
      TwoCentersSeededHelper(new_data_ball_0, new_data_ball_1)
    else (new_data_ball_0, new_data_ball_1)
  }

  // Cluster data using two centers method, given initial
  // guesses of the centroids.
  def TwoCentersSeeded[T <: Metric[T]](
      centroid_0: T, 
      centroid_1: T, 
      metrics: Set[T]): 
      (DataBall[T], DataBall[T]) = { 
    // TODO: This check is expensive and probably unnecessary.
    require(metrics.contains(centroid_0))
    require(metrics.contains(centroid_1))

    // // Initialize with everything in |cluster_0| except the
    // // centroid of |cluster_1|. 
    // val data_0 = metrics - centroid_1
    // val data_1 = Set(centroid_1)

   
    val data_ball_0 = DataBall(Ball(centroid_0, Double.MaxValue), metrics)
    val data_ball_1 = DataBall(Ball(centroid_1, Double.MaxValue), Set.empty[T])
    TwoCentersSeededHelper(data_ball_0, data_ball_1)
  }

  // Two centers clustering, selecting random elements 
  // to be the initial centroids.
  def TwoCenters[T <: Metric[T]](
	metrics: Set[T]): (DataBall[T], DataBall[T]) = {
    require(metrics.size >= 2)

    // TODO: This is inefficient.
    val shuffled = Util.random.shuffle(metrics.toList)
    val centroid_0 :: centroid_1 :: _ = shuffled.toList
    TwoCentersSeeded(centroid_0, centroid_1, metrics)
  }

  def RecursiveTwoCentersHelper[T <: Metric[T]](data_ball: DataBall[T]): Tree[Ball[T]] = {
    require(data_ball.size > 0)

    val DataBall(ball, data_with_center) = data_ball
    val data = data_with_center - ball.center

    val children = if (data.size == 0) { 
      List()
    } else if (data.size == 1) { 
      val child = Tree(Ball(data.head, 0), List())
      List(child)
    } else {
      val (data_ball_0, data_ball_1) = TwoCenters(data)
      val child_0 = RecursiveTwoCentersHelper(data_ball_0)
      val child_1 = RecursiveTwoCentersHelper(data_ball_1)
      List(child_0, child_1)
    }

    Tree(ball, children)
  }

  // Recursively clusters the data, producing a tree of Balls. The leafs of
  // the tree are Balls of radius 0, corresponding to individual elements.
  def RecursiveTwoCenters[T <: Metric[T]](metrics: Set[T]): Tree[Ball[T]] = { 
    val ball = MinCoveringBall(metrics)
    val data_ball = DataBall(ball, metrics)
    RecursiveTwoCentersHelper(data_ball)
  }
}

package mbtree

// A ball in a metric space.
case class Ball[T](val center: T, val radius: Double)

object Ball {
  // Returns the smallest ball containing the given points, constrained to
  // be centered on one of the given points.
  def apply[T <: Metric[T]](metrics: Set[T]): Ball[T] = {
    require(metrics.size > 0)

    def MaxDistance(metric: T): Double = 
        metrics.map(x => x.Distance(metric)).max

    val radii = metrics.map(MaxDistance)
    val (center, radius) = metrics.zip(radii).minBy(_._2)

    Ball(center, radius)
  }

  def InfiniteBall[T](center: T) = Ball(center, Double.MaxValue)

  def IsInfinite[T](ball: Ball[T]) = ball.radius == Double.MaxValue
}

// A DataBall is a set of data along with a ball that contains the data.
// The ball always contains the associated data.
case class DataBall[T] private (val ball: Ball[T], val data: Set[T]) { 
  val center = ball.center
  val radius = ball.radius
  val size = data.size
}

object DataBall { 
  def InfiniteDataBall[T <: Metric[T]](center: T, data: Set[T]): DataBall[T] = { 
    DataBall(Ball.InfiniteBall(center), data)
  }

  def apply[T <: Metric[T]](metrics: Set[T]): DataBall[T] = { 
    require(metrics.size > 0)

    DataBall(Ball(metrics), metrics)
  }
}

object Cluster { 
  def TwoCentersAssignToCenters[T <: Metric[T]](
      center_0: T, 
      center_1: T, 
      metrics: Set[T]): (Set[T], Set[T]) = {     
    metrics.partition(x => x.Distance(center_0) <= x.Distance(center_1))
  }

  def TwoCentersSeededHelper[T <: Metric[T]](
      data_ball_0: DataBall[T],
      data_ball_1: DataBall[T]): (DataBall[T], DataBall[T]) = {
    val (new_data_0, new_data_1) = 
	TwoCentersAssignToCenters(data_ball_0.center,
 				  data_ball_1.center, 
				  data_ball_0.data ++ data_ball_1.data)
    val new_data_ball_0 = DataBall(new_data_0)
    val new_data_ball_1 = DataBall(new_data_1)

    // Recursively call this function until convergence.
    val error = data_ball_0.radius + data_ball_1.radius
    val new_error = new_data_ball_0.radius + new_data_ball_1.radius
    if (new_error < error)
      TwoCentersSeededHelper(new_data_ball_0, new_data_ball_1)
    else (new_data_ball_0, new_data_ball_1)
  }

  // Cluster data using two centers method, given initial
  // guesses of the centroids.
  def TwoCentersSeeded[T <: Metric[T]](
      centroid_0: T, 
      centroid_1: T, 
      metrics: Set[T]): (DataBall[T], DataBall[T]) = {
    // Weird stuff would happen if the centroids were the same.
    require(centroid_0 != centroid_1)
    // This requirement isn't strictly necessary.
    require(metrics.contains(centroid_0))
    require(metrics.contains(centroid_1))

    val data_ball_0 = DataBall.InfiniteDataBall(centroid_0, metrics)
    val data_ball_1 = DataBall.InfiniteDataBall(centroid_1, Set.empty[T])
    TwoCentersSeededHelper(data_ball_0, data_ball_1)
  }

  // Two centers clustering, selecting random elements 
  // to be the initial centroids.
  def TwoCenters[T <: Metric[T]](
	metrics: Set[T]): (DataBall[T], DataBall[T]) = {
    require(metrics.size >= 2)

    val IndexedSeq(centroid_0, centroid_1) = Util.RandomSample(metrics.toIndexedSeq, 2)
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
    val data_ball = DataBall(metrics)
    RecursiveTwoCentersHelper(data_ball)
  }
}

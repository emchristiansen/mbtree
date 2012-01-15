package mbtree

// A basic tree data structure.
case class Tree[T](val data: T, val children: List[Tree[T]])

object Util {
  val random = new util.Random(0)

  class TimesDoClass(val n: Int) {
    def TimesDo(f: => Unit): Unit = (1 to n) foreach (_ => f)
    def TimesMap[T](f: => T): IndexedSeq[T] = (1 to n) map (_ => f)
  }

  implicit def ToTimesDoClass(n: Int) = new TimesDoClass(n)

  // Returns the insert location for the query assuming we're doing insertion sort.
  // Writing this when tired, and as simple as it is I think 
  // I may have screwed it up.
  def BinarySearch[T](query: T, sorted_data: IndexedSeq[T], IsLessThan: (T, T) => Boolean): Int = {
    // Invariant: The insert-sort index is always included in [min, max] (inclusive).
    def BinarySearchHelper(
      min: Int,
      max: Int): Int = {
      require(max > min)

      if (max == min + 1) {
        if (IsLessThan(query, sorted_data(min))) min
        else max
      } else {
        val mid = (max + min) / 2
        if (IsLessThan(query, sorted_data(mid))) BinarySearchHelper(min, mid)
        else BinarySearchHelper(mid, max)
      }
    }

    BinarySearchHelper(0, sorted_data.size)
  }

  class BoundedPriorityQueue[T](max_size: Int)(implicit ord: Ordering[T]) {
    val data = new collection.mutable.PriorityQueue[T]()(ord)

    def +=(element: T) {
      data += element
      if (data.size > max_size) data.dequeue
    }

    def AtCapacity = data.size == max_size

    def AsSortedList = data.toList.sorted(ord)
  }

  def RandomSubrangeHelper(
    range: Range,
    size: Int,
    selected: Set[Int]): Set[Int] = {
    require(range.size >= 2 * size)

    if (selected.size == size) selected
    else {
      val random_pick = (math.abs(random.nextInt) % (range.max + 1 - range.min)) + range.min
      RandomSubrangeHelper(range, size, selected + random_pick)
    }
  }

  // Returns a random subset of the given range of the given size.
  // So RandomSubrange(Range(0, 10), 4) might return (2, 3, 5, 8).
  def RandomSubrange(range: Range, size: Int): Set[Int] = {
    require(range.size >= size)

    // Use one of two methods according to the relative size of the subset.
    if (range.size < 2 * size) random.shuffle(range).take(size).toSet
    else RandomSubrangeHelper(range, size, Set())
  }

  // Returns a random subset of the given data of the given cardinality.
  def RandomSample[T](data: IndexedSeq[T], size: Int): IndexedSeq[T] = {
    require(data.size >= size)

    val random_indices = {
      val random_set = RandomSubrange(0 until data.size, size)
      random_set.toIndexedSeq.sortWith(_ < _)
    }
    assert(random_indices.size == size)

    // Poor man's slicing.
    for (index <- random_indices) yield data(index)
  }

  // Break into folds recursively; easier to code than how you might implement
  // standard k-fold cross validation.
  def BreakIntoFolds[T](
    data: IndexedSeq[T],
    fold_depth: Int): List[(IndexedSeq[T], IndexedSeq[T])] = {
    require(fold_depth >= 1)

    val (front, back) = data.splitAt(data.size / 2)
    if (fold_depth == 1) {
      List((front, back), (back, front))
    } else {
      // Can't be tail recursive because execution proceeds as a binary tree.
      val test_in_front = {
        val pairs = BreakIntoFolds(front, fold_depth - 1)
        for ((test, train) <- pairs) yield (test, train ++ back)
      }

      val test_in_back = {
        val pairs = BreakIntoFolds(back, fold_depth - 1)
        for ((test, train) <- pairs) yield (test, front ++ train)
      }

      test_in_front ++ test_in_back
    }
  }
}

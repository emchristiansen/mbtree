package mbtree

import collection.mutable
import util.Random

// A MultiSet is an unordered collection which can contain more than
// one of a given element.
// TODO: Make this look like a member of the collections library.
// See Chapter 25 of Programming in Scala 2nd Edition by Odersky.
class HashMapZero[T] extends mutable.HashMap[T, Int] { 
  override def default(key: T) = 0
}

class MultiSet[T] { 
  private val map = new HashMapZero[T]

  var Size = 0

  def Add(element: T) { 
    map(element) += 1
    Size += 1
  }

  def Decrement(element: T) { 
    assert(Contains(element))
    map(element) -= 1
    Size -= 1

    if (map(element) == 0) map.remove(element)
  }

  def Remove(element: T) { 
    assert(Contains(element))
    Size -= map(element)
    map.remove(element)
  }

  def Clone: MultiSet[T] = { 
    val clone_map = map.clone
    MultiSet(clone_map)
  }

  def Count(element: T) = map(element)

  def Contains(element: T) = Count(element) > 0

  def Partition(predicate: T => Boolean): (MultiSet[T], MultiSet[T]) = { 
    val (true_map, false_map) = map.partition(key_with_value => predicate(key_with_value._1))
    (MultiSet(true_map), MultiSet(false_map))
  }

  def ElementsNoCount = map.keys

  private def Add(map: mutable.HashMap[T, Int]) { 
    for ((element, count) <- map;
         _ <- 0 until count) { 
      Add(element)
    }
  }

  def ++(that: MultiSet[T]): MultiSet[T] = { 
    val out = new MultiSet[T]
    out.Add(map)
    out.Add(that.map)
    out
  }
}

object MultiSet { 
  def apply[T](map: mutable.HashMap[T, Int]): MultiSet[T] = { 
    val multi_set = new MultiSet[T]
    multi_set.Add(map)
    multi_set
  }

  def apply[T](elements: T*): MultiSet[T] = { 
    val multi_set = new MultiSet[T]
    elements.foreach(multi_set.Add)
    multi_set
  }
}

// A basic tree data structure, which can generate Dot source (Graphviz).
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

object Util { 
  val random = new Random(0)

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

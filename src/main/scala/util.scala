package mbtree

import collection.mutable
import util.Random

// A MultiSet is an unordered collection which can contain more than
// one of a given element.
// TODO: Make this look like a member of the collections library.
// See Chapter 25 of Programming in Scala 2nd Edition by Odersky.
class MultiSet[T](elements: T*) { 
  private val map = new mutable.HashMap[T, Int] {     
    override def default(key: T) = 0
  }

  def Add(element: T) { 
    map(element) += 1
  }

  def Count(element: T) = map(element)
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

package mbtree

import collection.mutable
import util.Random

// A MultiSet is an unordered collection which can contain more than
// one of a given element.
// TODO: Make this look like a member of the collections library.
class MultiSet[T](elements: T*) { 
  private val map = new mutable.HashMap[T, Int] {     
    def default(key: T) = 0
  }

  def Add(element: T) { 
    map(element) += 1
  }

  def Count(element: T) = map(element)
}

object Util { 
  val random = new Random(0)
}

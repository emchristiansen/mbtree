package mbtree

import scalax.io._

import ML._
import Util._

object IrisData extends App {
  val path = "/home/eric/cratch/uci_data/iris.data"
  val lines = Resource.fromFile(path).lines()

  val Pattern = """([\d\.])+,([\d\.])+,([\d\.])+,([\d\.])+.*""".r
  val data = for (Pattern(e1, e2, e3, e4) <- lines) yield
      List(e1.toDouble, e2.toDouble, e3.toDouble, e4.toDouble)

  val vectors = { 
    val unshuffled = for (d <- data.toList) yield L2Vector(d)
    random.shuffle(unshuffled)
  }

  val (queries, database) = vectors.splitAt(10)

  val tree = RecursiveTwoMeans(database)

//  println(tree.toDot)

  // val output = Resource.fromFile("/tmp/tree.dot")
  // output.write(tree.toDot)
}

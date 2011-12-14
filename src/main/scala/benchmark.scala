package mbtree

import Base._
import BenchmarkUtil._
import Util._

object BenchmarkBruteIris extends App {
  val metric_objects = { 
    val data = LoadIrisData
    val unshuffled = for (d <- data.toList) yield L2Vector(d)
    random.shuffle(unshuffled)
  }

//  val (total_time, num_metric_evals) = 
//      BenchmarkMethod((data: IndexedSeq[Metric[L2Vector]]) => new BruteNN(data), 
//		      metric_objects, 
//		      10)

  //val (queries, database) = vectors.splitAt(10)

//  val tree = RecursiveTwoMeans(database)

//  println(tree.toDot)

  // val output = Resource.fromFile("/tmp/tree.dot")
  // output.write(tree.toDot)
}

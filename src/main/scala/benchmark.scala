package mbtree

import Base._
import BenchmarkUtil._
import Util._

// TODO: Get rid of the "distinc" commands; it appears MBTreeNN has an infinite loop
// bug when there are redundant data. Or maybe it is just really slow? That's why
// I'm also just taking the first 50.
object BenchmarkBruteIris extends App {
  val metric_objects = { 
    val data = LoadIrisData
    val unshuffled = for (d <- data.toList) yield L2Vector(d)
    random.shuffle(unshuffled.distinct).take(32)
  }

  val (total_time, num_metric_evals) = Benchmark(
      (data: IndexedSeq[Metric[L2Vector]]) => new BruteNN(data), 
      metric_objects.toIndexedSeq, 
      3)

  println("total time, num metric evals: %.8f, %d".format(total_time, num_metric_evals))
}

object BenchmarkMBTreeIris extends App {
  val metric_objects = { 
    val data = LoadIrisData
    val unshuffled = for (d <- data.toList) yield L2Vector(d)
    random.shuffle(unshuffled.distinct).take(64)
  }

  val (total_time, num_metric_evals) = Benchmark(
      (data: IndexedSeq[Metric[L2Vector]]) => new MBTreeNN(data), 
      metric_objects.toIndexedSeq, 
      3)

  println("total time, num metric evals: %.8f, %d".format(total_time, num_metric_evals))
}

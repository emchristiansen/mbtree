package mbtree

import BenchmarkUtil._
import Util._

object BenchmarkBruteIris extends App {
  val metric_objects = { 
    val data = LoadIrisData
    val unshuffled = for (d <- data.toList) yield L2Vector(d)
    random.shuffle(unshuffled.distinct)
  }

  val (total_time, num_metric_evals) = Benchmark(
      (data: IndexedSeq[L2Vector]) => new BruteNN(data), 
      metric_objects.toIndexedSeq, 
      3)

  println("total time, num metric evals: %.8f, %d".format(total_time, num_metric_evals))
}

object BenchmarkMBTreeIris extends App {
  val metric_objects = { 
    val data = LoadIrisData
    val unshuffled = for (d <- data.toList) yield L2Vector(d)
    random.shuffle(unshuffled.distinct)
  }

  val (total_time, num_metric_evals) = Benchmark(
      (data: IndexedSeq[L2Vector]) => new MBTreeNN(data), 
      metric_objects.toIndexedSeq, 
      3)

  println("total time, num metric evals: %.8f, %d".format(total_time, num_metric_evals))
}

object RunBenchmark extends App {
  val data_filename = "yeast.data"
  def MakeMetric(d: List[Double]) = L2Vector(d)
  def MakeMethod[T <: Metric[T]](d: IndexedSeq[T]) = new MBTreeNN(d)

  val metric_objects = { 
    val data = LoadDoubleData(data_filename)
    val unshuffled = for (d <- data.toList) yield MakeMetric(d)
    random.shuffle(unshuffled.distinct)
  }

  val (total_time, num_metric_evals) = Benchmark(
      (data: IndexedSeq[L2Vector]) => MakeMethod(data), 
      metric_objects.toIndexedSeq, 
      3)

  println("total time, num metric evals: %.8f, %d".format(total_time, num_metric_evals))
}

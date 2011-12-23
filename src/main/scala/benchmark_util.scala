package mbtree

import scalax.io._

import Util._

object BenchmarkUtil { 
  val data_path = "data"

  def LoadIrisData = {
    val path = data_path + "/iris.data"
    val lines = Resource.fromFile(path).lines()

    val Pattern = """([\d\.])+,([\d\.])+,([\d\.])+,([\d\.])+.*""".r
    val data = for (Pattern(e1, e2, e3, e4) <- lines) yield
        List(e1.toDouble, e2.toDouble, e3.toDouble, e4.toDouble)
    data
  }

  def Benchmark[T <: Metric[T]](
      makeNN: (IndexedSeq[T]) => NNFinder[T], 
      test: IndexedSeq[T],
      train: IndexedSeq[T]): (Double, Int) = { 
    println("training")
    val nn_finder = makeNN(train)

    println("testing")
    val start = System.nanoTime
    val counts = test.map(t => nn_finder.FindNearestWithCount(t)._3)
    val seconds = (System.nanoTime - start).toDouble / 1000000000
    (seconds, counts.sum)
  }

  def Benchmark[T <: Metric[T]](
      makeNN: (IndexedSeq[T]) => NNFinder[T],
      data: IndexedSeq[T],
      fold_depth: Int): (Double, Int) = { 
    val folds = BreakIntoFolds(data, fold_depth)

    val scores = folds.map(f => Benchmark(makeNN, f._1, f._2))
    (scores.map(_._1).sum, scores.map(_._2).sum)
  }
}

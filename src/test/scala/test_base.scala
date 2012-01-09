package mbtree

import math._
import org.scalatest.FunSuite

import mbtree.extra._

class TestBase extends FunSuite {
  val v1 = L2Vector(0, 0, 0)
  val v2 = L2Vector(-1, -1, 1)
  val v3 = L2Vector(2, 2, 1)

  // TODO: Redo with ScalaCheck for sexy test autogeneration.
  test("test L2Vector calculates distances correctly") { 
    assert(v1.Distance(v1) == 0)
    assert(v1.Distance(v2) == sqrt(1 + 1 + 1))
    assert(v1.Distance(v3) == sqrt(4 + 4 + 1))

    assert(v2.Distance(v1) == v1.Distance(v2))
    assert(v2.Distance(v2) == 0)
    assert(v2.Distance(v3) == sqrt(9 + 9 + 0))
  }

  test("spot check BruteNN") { 
    val brute = new BruteNN(Array(v1, v2, v3))

    val (i1, d1) = brute.FindNearest(L2Vector(0, 0, .1))
    assert(d1 === .1); assert(i1 === 0)

    val (i2, d2) = brute.FindNearest(L2Vector(-1, -2, 4))
    assert(d2 === sqrt(1 + 9)); assert(i2 === 1)
  }
}

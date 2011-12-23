package mbtree

import org.scalatest.FunSuite

import Cluster._

class TestCluster extends FunSuite { 
  val a1 = L2Vector(1.0, 0.0)
  val a2 = L2Vector(10.0, 10.0)
  val a3 = L2Vector(10.0, 11.0)
  val a4 = L2Vector(0.0, 0.0)
  val a5 = L2Vector(2.0, 0.0)

  test("test assigning points to centers") { 
    val all_data = Set(a1, a2, a3, a4, a5)
    val (cluster_0, cluster_1) = 
  	TwoCentersAssignToCenters(a1, a2, all_data)

    assert(cluster_0.size === 3)
    assert(cluster_0.contains(a1))
    assert(cluster_0.contains(a4))
    assert(cluster_0.contains(a5))

    assert(cluster_1.size === 2)
    assert(cluster_1.contains(a2))
    assert(cluster_1.contains(a3))
  }

  test("find the centroid") { 
    val ball = MinCoveringBall(Set(a5, a1, a4))
    assert(ball.center === a1)
  }

  test("find the ball covering one point") { 
    assert(MinCoveringBall(Set(a2)) === Ball(a2, 0))
  }

  test("test 2 center") { 
    val (c1, c2) = TwoCentersSeeded(a1, a2, Set(a1, a2, a3, a4, a5))
    val m1 = c1.data
    val m2 = c2.data

    assert(m1.size === 3)
    assert(m1.contains(a1))
    assert(m1.contains(a4))
    assert(m1.contains(a5))

    assert(m2.size === 2)
    assert(m2.contains(a2))
    assert(m2.contains(a3))
  }

  def GetLeafs[T](tree: Tree[T]): List[T] = { 
    if (tree.children.isEmpty) List(tree.data)
    else { 
      val leaf_lists = tree.children.map(GetLeafs)
      leaf_lists.flatten
    }
  }

  def GetAllNodes[T](tree: Tree[T]): List[T] = { 
    if (tree.children.isEmpty) List(tree.data)
    else { 
      val leaf_lists = tree.children.map(GetAllNodes)
      tree.data :: leaf_lists.flatten
    }    
  }

  test("test recursive 2 centers") { 
    val data = Set(a1, a2, a3, a4, a5)
    val tree = RecursiveTwoCenters(data)

    println(tree.toDot)

    val leafs = GetAllNodes(tree)
    assert(leafs.size === data.size)
    assert(leafs.map(_.center).toSet == data.toSet)
  }
}

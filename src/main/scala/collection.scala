package mbtree

import collection._
import generic.CanBuildFrom
import mutable.{Builder, MapBuilder}

final class Bag[T] private (val map: Map[T, Int]) 
    extends Map[T, Int] with MapLike[T, Int, Bag[T]] {
  def get(key: T): Option[Int] = map.get(key)

  def iterator: Iterator[(T, Int)] = map.iterator

  // TODO: Do this without dynamic type checking and possible
  // exception throwing.
  def +[Bl >: Int](kv: (T, Bl)): Map[T, Bl] = kv match { 
    case (key: T, value: Int) => new Bag(map + kv.asInstanceOf[Tuple2[T, Int]])
    case _ => throw new Exception("Using bad types when creating a Bag.")
  }

  def -(key: T): Bag[T] = new Bag(map - key)

  override def empty: Bag[T] = Bag.empty

  val sizeNotDistinct = map.map(_._2).sum
}

object Bag { 
  def empty[T] = new Bag(Map.empty[T, Int])

  def apply[T](map: Map[T, Int]): Bag[T] = new Bag(map)

  def apply[T](data: T*): Bag[T] = { 
    val mutable_map = new mutable.HashMap[T, Int] { 
      override def default(key: T) = 0
    }
    data.foreach(x => mutable_map(x) += 1)
    new Bag(mutable_map.toMap)
  } 

  def newBuilder[T]: Builder[(T, Int), Bag[T]] = { 
    new MapBuilder[T, Int, Map[T, Int]](Map.empty[T, Int]) mapResult Bag.apply
  }

  implicit def canBuildFrom[T]: CanBuildFrom[Bag[_], (T, Int), Bag[T]] = { 
    new CanBuildFrom[Bag[_], (T, Int), Bag[T]] { 
      def apply(from: Bag[_]) = newBuilder[T]
      def apply = newBuilder[T]
    }
  }
}

object PirateMap { 
  def empty[A, B]: PirateMap[A, B] = new PirateMap(Map.empty[A, B])

  def apply[A, B](map: Map[A, B]): PirateMap[A, B] = new PirateMap(map)

  def newBuilder[A, B]: Builder[(A, B), PirateMap[A, B]] = { 
    new MapBuilder[A, B, Map[A, B]](Map.empty[A, B]) mapResult PirateMap.apply
  }

  implicit def canBuildFrom[A, B]: CanBuildFrom[PirateMap[_, _], (A, B), PirateMap[A, B]] = { 
    new CanBuildFrom[PirateMap[_, _], (A, B), PirateMap[A, B]] { 
      def apply(from: PirateMap[_, _]) = PirateMap.newBuilder[A, B]
      def apply = PirateMap.newBuilder[A, B]
    }
  }
}

final class PirateMap[A, B] private (map: Map[A, B]) 
    extends Map[A, B] with MapLike[A, B, PirateMap[A, B]] { 
  override def empty: PirateMap[A, B] = PirateMap.empty
  
  def get(key: A): Option[B] = map.get(key)
  def iterator: Iterator[(A, B)] = map.iterator

  def +[Bl >: B](kv: (A, Bl)): PirateMap[A, Bl] = new PirateMap(map + kv)

  def -(key: A): PirateMap[A, B] = new PirateMap(map - key)   
}

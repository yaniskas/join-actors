package join_patterns.util

import it.unimi.dsi.fastutil.ints.IntArrayList

import scala.annotation.targetName
import scala.collection.{Factory, IndexedSeqView, SeqView, View, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import os.truncate

class GrowingIntArrayRef(private val backing: IntArrayList, private val viewLength: Int) extends Iterable[Int], Seq[Int]:
  @targetName("colonPlus")
  def :+(elem: Int): GrowingIntArrayRef =
    if viewLength == backing.size then
      backing.add(elem)
      GrowingIntArrayRef(backing, viewLength + 1)
    else
      val newBacking = IntArrayList()
      var i = 0
      while i < viewLength do
        newBacking.add(this(i))
        i += 1
      newBacking.add(elem)
      GrowingIntArrayRef(newBacking, viewLength + 1)

  inline def apply(i: Int): Int =
    backing.getInt(i)

  override def equals(other: Any): Boolean =
    other match
      case other: GrowingIntArrayRef =>
        if this.viewLength != other.viewLength then false
        else
          var i = 0
          while i < viewLength do
            if this(i) != other(i) then return false
            i += 1
          true
      case _ => false

  def length: Int = viewLength

  // def size: Int = viewLength

  private def toView =
    backing.iterator().asScala.toBuffer.view.take(viewLength).asInstanceOf[SeqView[Int]]

  // def mkString(sep: String): String = toView.mkString(sep)

  // def mkString(start: String, sep: String, end: String): String = toView.mkString(start, sep, end)

  def contains(elem: Int): Boolean =
    var i = 0
    while i < viewLength do
      if this(i) == elem then return true
      i += 1
    false
  
  override def combinations(n: Int): Iterator[LazyList[Int]] = toView.combinations(n).map(_.to(LazyList))

  def sorted: GrowingIntArrayRef =
    toView.sorted.to(GrowingIntArrayRef)
//    GrowingIntArrayRef(newBacking, newBacking.length)

  override def iterator: Iterator[Int] = backing.iterator().asScala.take(viewLength).asInstanceOf[Iterator[Int]]

object GrowingIntArrayRef extends Factory[Int, GrowingIntArrayRef]:
  def apply(backing: IntArrayList, viewLength: Int): GrowingIntArrayRef =
    new GrowingIntArrayRef(backing, viewLength)

  def apply(elems: Int*): GrowingIntArrayRef =
    val backing = IntArrayList.of(elems*)
    GrowingIntArrayRef(backing, backing.size)

  def fromSpecific(it: IterableOnce[Int]): GrowingIntArrayRef =
    val javaIterator = it.iterator.asJava.asInstanceOf[java.util.Iterator[Integer]]
    val backing = IntArrayList(javaIterator)
    GrowingIntArrayRef(backing, backing.size)

  def newBuilder: mutable.Builder[Int, GrowingIntArrayRef] =
    ???


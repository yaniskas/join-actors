package join_patterns.util

import scala.collection.immutable.BitSet
import scala.collection.{Factory, mutable}

class OffsetBitSet(val minElement: Int, val underlying: BitSet) extends Set[Int]:
  override val size: Int = underlying.size

  override def incl(elem: Int): OffsetBitSet =
    if elem < minElement then throw UnsupportedOperationException()
    else
      val newUnderlying = underlying.incl(elem - minElement)
      new OffsetBitSet(minElement, newUnderlying)

  override def excl(elem: Int): OffsetBitSet = new OffsetBitSet(minElement, underlying.excl(elem - minElement))

  override def contains(elem: Int): Boolean = underlying.contains(elem - minElement)

  override def iterator: Iterator[Int] = underlying.iterator.map(_ + minElement)


object OffsetBitSet extends Factory[Int, OffsetBitSet]:
  def apply(elems: Int*): OffsetBitSet =
    if elems.isEmpty then new OffsetBitSet(0, BitSet())
    else
      val minElement = elems.min
      val underlying = BitSet(elems.map(i => i - minElement) *)

      new OffsetBitSet(minElement, underlying)

  def fromSpecific(it: IterableOnce[Int]): OffsetBitSet =
    ???

  def newBuilder: mutable.Builder[Int, OffsetBitSet] =
    ???

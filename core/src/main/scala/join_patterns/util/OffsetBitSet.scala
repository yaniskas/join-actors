package join_patterns.util

import scala.collection.immutable.{ArraySeq, BitSet}
import scala.collection.{Factory, mutable}

class OffsetBitSet(val minElement: Int, val underlying: BitSet, override val size: Int) extends Set[Int]:
  override def incl(elem: Int): OffsetBitSet =
    if underlying.isEmpty then
      new OffsetBitSet(elem, BitSet(0), 1)
    else if elem >= minElement then
      val newUnderlying = underlying.incl(elem - minElement)
      new OffsetBitSet(minElement, newUnderlying, size + 1)
    else (this.iterator ++ Iterator(elem)).to(OffsetBitSet)

  override def excl(elem: Int): OffsetBitSet = new OffsetBitSet(minElement, underlying.excl(elem - minElement), size - 1)

  override def contains(elem: Int): Boolean = underlying.contains(elem - minElement)

  override def iterator: Iterator[Int] = underlying.iterator.map(_ + minElement)

  override def toString(): String = underlying.toString()

  def toArraySeqFast: ArraySeq[Int] =
    val array = underlying.toArray

    var i = 0
    while i < array.length do
      array(i) += minElement
      i += 1

    ArraySeq.ofInt(array)

  def compareFirstDifferent(other: OffsetBitSet): Int =
    // Inspired by https://stackoverflow.com/questions/27331175/java-bitset-comparison

    if this.minElement != other.minElement then throw IllegalArgumentException()

    val newUnderlying = underlying.xor(other.underlying)
    if newUnderlying.isEmpty then 0
    else
      val newUnderlyingMin = underlying.min
      if other.contains(newUnderlyingMin) then 1 else -1


//    new OffsetBitSet(newMin, newUnderlying, newUnderlying.size)

object OffsetBitSet extends Factory[Int, OffsetBitSet]:
  def apply(elems: Int*): OffsetBitSet =
    if elems.isEmpty then new OffsetBitSet(0, BitSet(), 0)
    else
      val minElement = elems.min
      val underlying = BitSet(elems.map(i => i - minElement) *)

      new OffsetBitSet(minElement, underlying, elems.length)

  def fromSpecific(it: IterableOnce[Int]): OffsetBitSet =
    val arr = it.iterator.toArray
    val minElement = arr.min
    new OffsetBitSet(minElement, arr.map(_ - minElement).to(BitSet), arr.length)

  def newBuilder: mutable.Builder[Int, OffsetBitSet] =
    ???

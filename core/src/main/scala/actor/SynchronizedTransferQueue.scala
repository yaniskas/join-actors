//package actor
//
//import scala.collection.mutable
//
//class SynchronizedTransferQueue[M]:
//  private val delegate = mutable.ArrayDeque[M]()
//
//  def put(m: M): Unit =
//    synchronized:
//      delegate.append(m)
//      notifyAll()
//
//  def take(): M =
//    synchronized:
//      while delegate.isEmpty do wait()
//      delegate.removeHead(false)
//
//  def prependAll(msgs: IterableOnce[M]): Unit =
//    synchronized:
//      delegate.prependAll(msgs)

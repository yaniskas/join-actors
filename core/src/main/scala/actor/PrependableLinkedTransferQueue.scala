package actor

import java.util.concurrent.LinkedTransferQueue
import scala.collection.mutable

class PrependableLinkedTransferQueue[M]:
  private val delegate = LinkedTransferQueue[M]()
  private val prepends = mutable.ArrayDeque[M]()

  def put(m: M): Unit =
    delegate.put(m)

  def take(): M =
    if prepends.nonEmpty then prepends.removeHead(false)
    else delegate.take()

  def prependAll(msgs: IterableOnce[M]): Unit =
    prepends.prependAll(msgs)

package join_patterns.matching.mixin

import scala.collection.mutable.Map as MutMap

trait MutableMapMessageStore[M]:
  // Messages extracted from the queue are saved here to survive across apply() calls
  protected val messages: MutMap[Int, M] = MutMap()

  def storedMessages: IterableOnce[M] = messages.toList.sortBy(_._1).map(_._2)

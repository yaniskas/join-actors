package join_patterns.matching.filtering_parallel

import join_actors.actor.*
import join_patterns.matching.mixin.MutableMapMessageStore
import join_patterns.matching.{CandidateMatchOpt, CandidateMatches, Matcher}
import join_patterns.types.JoinPattern
import join_patterns.util.*

import scala.collection.mutable.{ArrayBuffer, HashMap as MutableHashMap}

class FilteringParallelMatcher[M, T](private val patterns: List[JoinPattern[M, T]], numThreads: Int) extends Matcher[M, T], MutableMapMessageStore[M]:

  private var nextMessageIndex = 0

  private val matchingTrees: List[FilteringParallelMatchingTree[M, T]] =
    patterns.zipWithIndex.map(FilteringParallelMatchingTree(_, _, numThreads))


  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =
    var result: Option[T] = None

    while result.isEmpty do
      val msg = q.take()
      val index = nextMessageIndex
      nextMessageIndex += 1

      messages.update(index, msg)

      val matches = ArrayBuffer[CandidateMatchOpt[M, T]]()
      for tree <- matchingTrees.fast do matches.append(tree.findMatch(index, msg, messages))

      val candidateMatches: CandidateMatches[M, T] =
        matches.foldLeft(CandidateMatches[M, T]()) {
          case (acc, Some(candidateMatch)) =>
            val (msgIdxs, p) = candidateMatch
            acc.updated(msgIdxs, p)
          case (acc, None) => acc
        }

      if candidateMatches.nonEmpty then
        val ((candidateQidxs, patIdx), (substs, rhsFn)) = candidateMatches.head
        result = Some(rhsFn(substs, selfRef))

        // Prune tree
        for tree <- matchingTrees.fast do
          tree.pruneTree(candidateQidxs)

        // Remove selected message indices from messages
        for idx <- candidateQidxs.fast do
          messages.remove(idx)

    result.get

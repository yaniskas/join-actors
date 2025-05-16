package join_patterns.matching.mutable

import join_actors.actor.*
import join_patterns.matching.mixin.MutableMapMessageStore
import join_patterns.matching.{CandidateMatches, Matcher}
import join_patterns.types.JoinPattern

import scala.collection.mutable.HashMap as MutableHashMap

class MutableStatefulMatcher[M, T](private val patterns: List[JoinPattern[M, T]]) extends Matcher[M, T], MutableMapMessageStore[M]:

  private var nextMessageIndex = 0

  private val matchingTrees: List[MutableStatefulMatchingTree[M, T]] =
    patterns.zipWithIndex.map(MutableStatefulMatchingTree(_, _))


  def apply(q: Mailbox[M])(selfRef: ActorRef[M]): T =
    var result: Option[T] = None

    while result.isEmpty do
      val msg = q.take()
      val index = nextMessageIndex
      nextMessageIndex += 1

      messages.update(index, msg)

      val matches = matchingTrees.map(_.findMatch(index, msg, messages))

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
        for matchingTree <- matchingTrees do
          matchingTree.pruneTree(candidateQidxs)

        // Remove selected message indices from messages
        candidateQidxs.foreach { idx =>
          messages.remove(idx)
        }

    result.get

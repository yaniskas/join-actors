package join_patterns.matching.lazy_mutable

import join_actors.actor.ActorRef
import join_patterns.matching.CandidateMatchOpt
import join_patterns.matching.functions.*
import join_patterns.types.{JoinPattern, LookupEnv, MessageIdxs, PatternBins, PatternIdxs, given}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, Map as MutableMap, TreeMap as MutableTreeMap}
import scala.util.boundary
import scala.util.boundary.break

class LazyMutableMatchingTree[M, T](private val pattern: JoinPattern[M, T], private val patternIdx: Int):
  private val patternExtractors = pattern.getPatternInfo.patternExtractors

  private val nodes = MutableTreeMap[MessageIdxs, PatternBins](MessageIdxs() -> pattern.getPatternInfo.patternBins)


  private def updateTree(newMessageIdx: Int, msg: M, messages: MutableMap[Int, M]): CandidateMatchOpt[M, T] =

    val matchingConstructorIdxs = patternExtractors.iterator
      .filter { case (_idx, (msgTypeChecker, _msgFieldExtractor)) => msgTypeChecker(msg) }
      .map { (idx, _) => idx }
      .to(PatternIdxs)

    if matchingConstructorIdxs.isEmpty then None
    else
      val additions = ArrayBuffer[(MessageIdxs, PatternBins)]()

      val res = boundary:
        for (messageIdxsMatched, bins) <- nodes do
          // Create the child for one leaf in the matching tree

          // If the PatternBins contains a key for the constructor type of the new message, we might be able to compute a child
          bins.get(matchingConstructorIdxs).foreach { mappedMessageIdxs =>

            // We only add a new node if some of the constructor instances in the pattern don't already have a match
            if mappedMessageIdxs.size < matchingConstructorIdxs.size then
              val newMessageIdxs = messageIdxsMatched :+ newMessageIdx
              val newPatternBins = bins.updated(matchingConstructorIdxs, mappedMessageIdxs :+ newMessageIdx)

              if newMessageIdxs.size == pattern.size
                && newPatternBins.forall((patShapeSize, msgIdxs) => patShapeSize.size == msgIdxs.size)
              then
                // Find optimal permutation
                val bestPermutation = findBestValidPermutation(newPatternBins, messages)

                // If the guard can be satisfied, we break out of the loop with this permutation
                // Otherwise, we do nothing. Either way, we do not add a new node to the tree
                bestPermutation match
                  case r @ Some(_) =>
                    break(r)
                  case None => ()
              else
                val newNode = (newMessageIdxs, newPatternBins)
                additions.append(newNode)
          }

        // If the loop does not find any full nodes with a valid permutation, we do not have a result
        None

      res match
        case Some((bestMatchIdxs, bestMatchSubsts)) =>
          val selectedMatch =
            (
              bestMatchSubsts,
              (substs: LookupEnv, self: ActorRef[M]) => pattern.rhs(substs, self)
            )

          Some((bestMatchIdxs, patternIdx), selectedMatch)
        case None =>
          // We only add nodes to the tree if no match was found
          nodes.addAll(additions)
          None

  def findMatch(index: Int, msg: M, messages: MutableMap[Int, M]): CandidateMatchOpt[M, T] =
    updateTree(index, msg, messages)

  private def findBestValidPermutation(patternBins: PatternBins, messages: MutableMap[Int, M]): Option[(MessageIdxs, LookupEnv)] =
    val validPermutations =
      getMsgIdxsWithPayloadExtractor(patternExtractors, patternBins)
    val bestMatchOpt = findFairestMatch(validPermutations, messages, pattern)
    bestMatchOpt

  def pruneTree(messageIdxsToRemove: MessageIdxs): Unit =
    nodes.filterInPlace: (messageIdxs, _) =>
      messageIdxsToRemove.forall(i => !messageIdxs.contains(i))

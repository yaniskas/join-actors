package join_patterns.matching.trie_rec

import join_actors.actor.ActorRef
import join_patterns.matching.CandidateMatchOpt
import join_patterns.matching.functions.*
import join_patterns.matching.trie_while.TrieWhileMatchingTree.MatchingTrieNode
import join_patterns.types.{*, given}
import join_patterns.util.*

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map as MutableMap, TreeMap as MutableTreeMap}
import scala.util.boundary
import scala.util.boundary.break

class TrieRecMatchingTree[M, T](private val pattern: JoinPattern[M, T], private val patternIdx: Int):
  private val patternExtractors = pattern.getPatternInfo.patternExtractors

  private val rootNode = MatchingTrieNode(-1, pattern.getPatternInfo.patternBins, ArrayBuffer())

  private def traverseTrieH(
    currentNode: MatchingTrieNode,
    additions: ArrayBuffer[(MatchingTrieNode, MatchingTrieNode)],
    depth: Int,
    matchingConstructorIdxs: PatternIdxs,
    newMessageIdx: Int,
    msg: M,
    messages: MutableMap[Int, M]
  ): Option[(MessageIdxs, LookupEnv)] =
    // Create the child for one leaf in the matching tree
    // If the PatternBins contains a key for the constructor type of the new message, we might be able to compute a child
    val mappedMessageIdxs = currentNode.patternBins(matchingConstructorIdxs)

    // We only add a new node if some of the constructor instances in the pattern don't already have a match
    if mappedMessageIdxs.size < matchingConstructorIdxs.size then
      val newPatternBins = currentNode.patternBins.updated(matchingConstructorIdxs, mappedMessageIdxs :+ newMessageIdx)

      if depth + 1 == pattern.size
        && newPatternBins.forall((patShapeSize, msgIdxs) => patShapeSize.size == msgIdxs.size)
      then
        // Find optimal permutation
        val bestPermutation = findBestValidPermutation(newPatternBins, messages)

        // If the guard can be satisfied, we break out of the loop with this permutation
        // Otherwise, we do nothing. Either way, we do not add a new node to the tree
        return bestPermutation
      else
        val newNode = MatchingTrieNode(newMessageIdx, newPatternBins, ArrayBuffer())
        //                  println(s"Adding new node: $newNode")
        additions.append((currentNode, newNode))

    if currentNode.children.nonEmpty then
      for child <- currentNode.children.fast do
        val res = traverseTrieH(child, additions, depth + 1, matchingConstructorIdxs, newMessageIdx, msg, messages)
        res match
          case Some(r) => return Some(r)
          case None => ()

    None


  private def traverseTrie(newMessageIdx: Int, msg: M, messages: MutableMap[Int, M]): CandidateMatchOpt[M, T] =
//    println(s"Received message with index $newMessageIdx")
    val matchingConstructorIdxs = patternExtractors.iterator
      .filter { case (_idx, PatternIdxInfo(msgTypeChecker, _msgFieldExtractor, _)) => msgTypeChecker(msg) }
      .map { (idx, _) => idx }
      .to(PatternIdxs)

    if !rootNode.patternBins.contains(matchingConstructorIdxs) then None
    else
      val additions = ArrayBuffer[(MatchingTrieNode, MatchingTrieNode)]()

      val res = traverseTrieH(rootNode, additions, 0, matchingConstructorIdxs, newMessageIdx, msg, messages)

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
          for (parent, child) <- additions.fast do
            parent.children.append(child)

          None

  def findMatch(index: Int, msg: M, messages: MutableMap[Int, M]): CandidateMatchOpt[M, T] =
    traverseTrie(index, msg, messages)

  private def findBestValidPermutation(patternBins: PatternBins, messages: MutableMap[Int, M]): Option[(MessageIdxs, LookupEnv)] =
    val validPermutations =
      getMsgIdxsWithPayloadExtractor(patternExtractors, patternBins)
    val bestMatchOpt = findFairestMatch(validPermutations, messages, pattern)
    bestMatchOpt

  def pruneTree(messageIdxsToRemove: MessageIdxs): Unit =
    pruneTreeTraverse(rootNode, messageIdxsToRemove)

  private def pruneTreeTraverse(node: MatchingTrieNode, messageIdxsToRemove: MessageIdxs): Unit =
    node.children.filterInPlace(mtn => !messageIdxsToRemove.contains(mtn.messageIdx))
    for n <- node.children.fast do
      pruneTreeTraverse(n, messageIdxsToRemove)

object TrieRecMatchingTree:
  final case class MatchingTrieNode(messageIdx: Int, patternBins: PatternBins, children: ArrayBuffer[MatchingTrieNode])

package test.utils

import join_actors.api.*
import join_patterns.matching.trie_rec.TrieRecMatcher
import join_patterns.matching.trie_while.TrieWhileMatcher
import org.scalatest.prop.TableDrivenPropertyChecks.Table

val matchers = Table(
  "Matcher",
  ArrayParallelMatcher(2),
  ArrayWhileMatcher,
  BruteForceMatcher,
  EagerParallelMatcher(2),
  FilteringParallelMatcher(2),
  FilteringWhileMatcher,
  LazyMutableMatcher,
  LazyParallelMatcher(2),
  MutableStatefulMatcher,
  StatefulTreeMatcher,
  WhileEagerMatcher,
  WhileLazyMatcher,
  TrieWhileMatcher,
  TrieRecMatcher
)

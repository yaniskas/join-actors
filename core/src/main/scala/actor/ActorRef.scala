package actor

import java.util.concurrent.LinkedTransferQueue
import join_patterns.Matcher

/** Represents a reference to an actor.
  *
  * @param q
  *   The underlying queue used for message passing. This is a blocking queue that blocks its thread
  *   when the queue is empty.
  * @tparam M
  *   The type of messages that can be sent to the actor.
  */
class ActorRef[M, T](actor: Actor[M, T]):
  /** Sends a message to the actor.
    *
    * @param m
    *   The message to be sent.
    */
  def send(m: M): Unit = actor.receive(m)

  /** Sends a message to the actor using the `!` operator.
    *
    * @param m
    *   The message to be sent.
    */
  def !(m: M): Unit = send(m)

  export actor.requestMatcherSwitch

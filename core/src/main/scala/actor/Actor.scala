package actor

import join_patterns.Matcher

import java.util.concurrent.Executors
import java.util.concurrent.LinkedTransferQueue as Mailbox
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.*
import scala.jdk.CollectionConverters.*
import java.util.stream.{Collectors, Stream}

implicit val ec: ExecutionContext =
  ExecutionContext.fromExecutorService(
    Executors.newVirtualThreadPerTaskExecutor()
  )

enum Result[+T]:
  case Stop(value: T)
  case Continue

import Result.*

/** Represents an actor that processes messages of type M and produces a result of type T.
  *
  * @param matcher
  *   A matcher is the object that performs the join pattern matching on the messages in the actor's
  *   mailbox.
  * @tparam M
  *   The type of messages processed by the actor.
  * @tparam T
  *   The type of result produced by the actor. Which is the right-hand side of the join pattern.
  */
class Actor[M, T](private var matcher: Matcher[M, Result[T]]):
  private var mailbox: Mailbox[M] = Mailbox[M]
  private val ref                = ActorRef(this)

  /** Starts the actor and returns a future that will be completed with the result produced by the
    * actor, and the actor reference.
    *
    * @return
    *   A tuple containing the future result and the actor reference.
    */
  def start(): (Future[T], ActorRef[M, T]) =
    val promise = Promise[T]

    ec.execute(() => run(promise))

    (promise.future, ref)

  /** Runs the actor's message processing loop recursively until a stop signal is received, and
    * completes the provided promise with the resulting value.
    *
    * @param promise
    *   The promise to be completed with the actor's result.
    */
  @tailrec
  private def run(promise: Promise[T]): Unit =
    matcher(mailbox)(ref) match
      case Continue    => run(promise)
      case Stop(value) => promise.success(value)

  def receive(msg: M): Unit =
    mailbox.put(msg)

  def switchMatcher(newMatcher: Matcher[M, Result[T]]): Result[T] =
    val extractedMessages = this.matcher.extractedMessages

    val newMailbox = Stream.concat(
      extractedMessages.iterator.toList.asJava.stream(),
      mailbox.stream()
    ).collect(Collectors.toCollection{ () => Mailbox[M] })

    this.matcher = newMatcher
    this.mailbox = newMailbox

    matcher(mailbox)(ref)

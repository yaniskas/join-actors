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

/**
 * A reference cell type for defining mutually recursive matchers
 * @param con
 *  The initial contents of the reference cell, optional
 * @tparam T
 *  The type of the reference cell's contents
 */
class RefCell[T] private (private var _content: Option[T]):
  def this(initialContent: T) =
    this(Some(initialContent))
  
  def this() =
    this(None)
  
  def content_=(con: T) =
    _content = Some(con)
  
  def content =
    _content.get

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
class Actor[M, T](private var matcher: Matcher[M, T]):
  private var mailbox: Mailbox[M] = Mailbox[M]
  private val ref                = ActorRef(this)

  private var requestedNewMatcher: Option[Matcher[M, T]] = None

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
      case Continue    =>
        // Switch matcher if requested
        requestedNewMatcher.foreach { newMatcher =>
          val extractedMessages = this.matcher.extractedMessages

          val newMailbox = Stream.concat(
            extractedMessages.iterator.toList.asJava.stream(),
            mailbox.stream()
          ).collect(Collectors.toCollection{ () => Mailbox[M] })

          this.matcher = newMatcher
          this.mailbox = newMailbox
          this.requestedNewMatcher = None
        }
        
        run(promise)
      case Stop(value) => promise.success(value)

  /**
    * Places a message in the actor's mailbox
    *
    * @param msg
    *   The message to be sent.
    */
  def receive(msg: M): Unit =
    mailbox.put(msg)

  /**
    * Request the actor to switch matcher. The switch will be applied after the current RHS executes
    */
  def requestMatcherSwitch(newMatcher: Matcher[M, T]): Unit =
    this.requestedNewMatcher = Some(newMatcher)

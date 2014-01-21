/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case Insert(requester, id, elem) => root ! Insert(requester, id, elem)
    case Remove(requester, id, elem) => root ! Remove(requester, id, elem)
    case Contains(requester, id, elem) => root ! Contains(requester, id, elem)
    case GC =>
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    case _ => throw new IllegalAccessError()
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case i: Insert => pendingQueue = pendingQueue.enqueue(i)
    case r: Remove => pendingQueue = pendingQueue.enqueue(r)
    case c: Contains => pendingQueue = pendingQueue.enqueue(c)
    case CopyFinished =>
      root ! PoisonPill
      root = newRoot
      context.become(normal)
//      processQueue(pendingQueue)
      pendingQueue foreach (newRoot ! _)
      pendingQueue = Queue.empty[Operation]
    case GC =>
    case _ => throw new IllegalAccessError()
  }

  def processQueue(queue: Queue[Operation]): Unit = {
    if (queue.nonEmpty) {
      val (op, q) = queue.dequeue
      root ! op
      processQueue(q)
    }
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, otherElem) =>
      if (otherElem == elem) {
        if (removed) removed = false
        requester ! OperationFinished(id)
      }
      else if (otherElem < elem) {
        if (subtrees.contains(Left)) subtrees(Left) ! Insert(requester, id, otherElem)
        else {
          subtrees += Left -> context.actorOf(BinaryTreeNode.props(otherElem, initiallyRemoved = false))
          requester ! OperationFinished(id)
        }
      }
      else {
        if (subtrees.contains(Right)) subtrees(Right) ! Insert(requester, id, otherElem)
        else {
          subtrees += Right -> context.actorOf(BinaryTreeNode.props(otherElem, initiallyRemoved = false))
          requester ! OperationFinished(id)
        }
      }
    case Remove(requester, id, otherElem) =>
      if (otherElem == elem) {
        removed = true
        requester ! OperationFinished(id)
      }
      else if (otherElem < elem) {
        if (subtrees.contains(Left)) subtrees(Left) ! Remove(requester, id, otherElem)
        else {
          requester ! OperationFinished(id)
        }
      }
      else {
        if (subtrees.contains(Right)) subtrees(Right) ! Remove(requester, id, otherElem)
        else {
          requester ! OperationFinished(id)
        }
      }
    case Contains(requester, id, otherElem) =>
      if (otherElem == elem) requester ! ContainsResult(id, result = !removed)
      else if (otherElem < elem) {
        if (subtrees.contains(Left)) subtrees(Left) ! Contains(requester, id, otherElem)
        else requester ! ContainsResult(id, result = false)
      }
      else {
        if (subtrees.contains(Right)) subtrees(Right) ! Contains(requester, id, otherElem)
        else requester ! ContainsResult(id, result = false)
      }
    case CopyTo(node) =>
      for (child <- subtrees.values) {
        child ! CopyTo(node)
      }
      if (!removed) {
        node ! Insert(self, 0, elem)
      }
      if (removed && subtrees.isEmpty) context.parent ! CopyFinished
      else context.become(copying(subtrees.values.toSet, insertConfirmed = removed))
//    case PoisonPill =>
//      subtrees.values foreach (_ ! PoisonPill)
//      context.stop(self)
    case _ => throw new IllegalAccessError()
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(0) =>
      if (expected.isEmpty) {
        context.parent ! CopyFinished
        context.become(normal)
      } else {
        context.become(copying(expected, insertConfirmed = true))
      }
    case CopyFinished =>
      val newExpected = expected - sender
      if (newExpected.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
        context.become(normal)
      } else {
        context.become(copying(newExpected, insertConfirmed))
      }
    case _ => throw new IllegalAccessError()
  }

}

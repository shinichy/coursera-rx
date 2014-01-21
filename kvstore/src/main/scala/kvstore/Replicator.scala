package kvstore

import akka.actor.{Cancellable, Props, Actor, ActorRef}
import scala.concurrent.duration._
import scala.language.postfixOps

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]

  var cancellableMap = Map.empty[Long, Cancellable]

  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case Replicate(key, valueOp, id) =>
      println(s"[replicator] received Replicate($key, $valueOp, id = $id)")
      val seq = nextSeq
      acks += seq -> (sender, Replicate(key, valueOp, id))
      val cancellable = context.system.scheduler.schedule(0 milliseconds, 100 milliseconds) {
        println(s"[replicator] send Snapshot($key, $valueOp, seq = $seq)")
        replica ! Snapshot(key, valueOp, seq)
      }
      cancellableMap += seq -> cancellable
    case SnapshotAck(key, seq) =>
      println(s"[replicator] received SnapshotAck($key, seq = $seq)")
      // first success
      if (cancellableMap.contains(seq)) {
        val (sender, replicate) = acks(seq)

        // remove Replicate requests that has same id of this seq number
        val sameIdSeqs = for (
          (seq, (actor, rep)) <- acks
          if replicate.id == rep.id
        ) yield seq
        println(s"sameIdSeqs: $sameIdSeqs")
        sameIdSeqs foreach (cancellableMap(_).cancel())
        cancellableMap --= sameIdSeqs
        acks --= sameIdSeqs

        println(s"[replicator] send Replicated(${replicate.key}, id = ${replicate.id})")
        sender ! Replicated(replicate.key, replicate.id)
      } else {
        println(s"[replicator] this seq number is already acknowledged. seq: $seq")
      }
    case _ =>
  }

}

package kvstore

import akka.actor._
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.{Resume, Restart}
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import scala.concurrent.duration._
import akka.util.Timeout
import akka.actor.OneForOneStrategy
import scala.language.postfixOps

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var currentSeq: Long = 0L

  var persister: ActorRef = _

  var cancellableMapForPersist = Map.empty[Long, Cancellable]
  var cancellableMapForFailed = Map.empty[Long, Cancellable]
  var cancellableMapForReplicate = Map.empty[(ActorRef, Long), Cancellable]

  var finishedPersistedIds = Set.empty[Long]

  override def preStart(): Unit = {
    arbiter ! Join
    persister = context.actorOf(persistenceProps)
  }

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
    case _: PersistenceException =>
      println("PersistenceException")
      Restart
  }

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  def cleanup(id: Long) {
    println("[primary] cleanup")
    finishedPersistedIds -= id

    cancellableMapForFailed.get(id).foreach(_.cancel())
    cancellableMapForFailed -= id

    cancellableMapForPersist.get(id).foreach(_.cancel())
    cancellableMapForPersist -= id

    val cancellables = for (
      ((rep, idOfKey), cancellable) <- cancellableMapForReplicate
      if idOfKey == id
    ) yield (rep, id)

    cancellables foreach { key =>
      cancellableMapForReplicate(key).cancel()
      cancellableMapForReplicate -= key
    }
  }

  def sendAck(id: Long) {
    if (finishedPersistedIds.contains(id) && cancellableMapForReplicate.isEmpty) {
      println(s"[primary] send OperationAck($id)")
      cleanup(id)
      replicators foreach (_ ! OperationAck(id))
      // todo: is it ok to clear?
      replicators = Set.empty[ActorRef]
    } else {
      assert(!finishedPersistedIds.contains(id) || !cancellableMapForReplicate.isEmpty, s"persisted: ${finishedPersistedIds.contains(id)}, cancellableForReplicate size: ${cancellableMapForReplicate.size}")
      println(s"[primary] still waiting. persisted: ${finishedPersistedIds.contains(id)}, cancellableMapForReplicate size: ${cancellableMapForReplicate.size}")
    }
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Insert(key, value, id) =>
      println(s"[primary] received Insert($key, $value, $id)")
      kv += key -> value
      replicators += sender
      val cancellableForPersist = context.system.scheduler.schedule(0 millisecond, 100 millisecond) {
        println(s"[primary] send Persist($key, Some($value), $id)")
        persister ! Persist(key, Some(value), id)
      }
      cancellableMapForPersist += id -> cancellableForPersist

      secondaries.values foreach { replicator =>
        val cancellable = context.system.scheduler.schedule(0 millisecond, 100 millisecond) {
          println(s"[primary] send Replicate($key, Some($value), $id)")
          replicator ! Replicate(key, Some(value), id)
        }
        cancellableMapForReplicate += (replicator, id) -> cancellable
      }
      val cancellableForFailed = context.system.scheduler.scheduleOnce(1 second) {
        println(s"[primary] send OperationFailed($id)")
        replicators foreach (_ ! OperationFailed(id))
        cleanup(id)
      }
      cancellableMapForFailed += id -> cancellableForFailed
    case Remove(key, id) =>
      println(s"[primary] received Remove($key, $id)")
      kv -= key
      replicators += sender
      val cancellableForPersist = context.system.scheduler.schedule(0 millisecond, 100 millisecond) {
        println(s"[primary] send Persist($key, None, $id)")
        persister ! Persist(key, None, id)
      }
      cancellableMapForPersist += id -> cancellableForPersist

      secondaries.values foreach { replicator =>
        val cancellable = context.system.scheduler.schedule(0 millisecond, 100 millisecond) {
          println(s"[primary] send Replicate($key, None, $id)")
          replicator ! Replicate(key, None, id)
        }
        cancellableMapForReplicate += (replicator, id) -> cancellable
      }
      val cancellableForFailed = context.system.scheduler.scheduleOnce(1 second) {
        println(s"[primary] send OperationFailed($id)")
        replicators foreach (_ ! OperationFailed(id))
        cleanup(id)
      }
      cancellableMapForFailed += id -> cancellableForFailed
    case Persisted(key, id) =>
      println(s"[primary] received Persisted($key, $id)")
      finishedPersistedIds += id
      sendAck(id)
    case Replicated(key, id) =>
      println(s"[primary] received Replicated($key, $id)")
      cancellableMapForReplicate.get((sender, id)).foreach(_.cancel())
      cancellableMapForReplicate -= sender -> id
      sendAck(id)
    case Get(key, id) =>
      println(s"[primary] received Get(key = $key, id = $id)")
      println(s"[primary] send GetResult(key = $key, valueOption = ${kv.get(key)}}, id = $id)")
      sender ! GetResult(key, kv.get(key), id)
    case Replicas(replicas: Set[ActorRef]) =>
      println(s"[primary] received Replicas. size: ${replicas.size}")
      val removedReplicas = secondaries.keys.filterNot(replicas.contains)
      println(s"[primary] removed replicas: ${removedReplicas.size}")
      removedReplicas.foreach { replica =>
        val replicator: ActorRef = secondaries(replica)
        val cancellables = for (
          ((rep, id), cancellable) <- cancellableMapForReplicate
          if rep == replicator
        ) yield (rep, id)

        cancellables foreach { key =>
          cancellableMapForReplicate(key).cancel()
          cancellableMapForReplicate -= key
        }
        replicator ! PoisonPill
        secondaries -= replica
      }

      val newReplicas = replicas.filter(r => r != self && !secondaries.keys.toList.contains(r))
      println(s"[primary] new replicas: ${newReplicas.size}")
      newReplicas foreach { secondary =>
        val replicator = context.system.actorOf(Replicator.props(secondary))
        secondaries += secondary -> replicator
        kv foreach { pair =>
          println(s"[primary] send Replicate(${pair._1}, Some(${pair._2}), $currentSeq)")
          // todo: is it better to retry periodically?
          replicator ! Replicate(pair._1, Some(pair._2), currentSeq)
          finishedPersistedIds += currentSeq
          currentSeq += 1
        }
      }
    case _ =>
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key, id) =>
      sender ! GetResult(key, kv.get(key), id)
    case Snapshot(key, valueOp, seq) =>
      println(s"[secondary] received Snapshot($key, $valueOp, seq = $seq)")
      if (seq > currentSeq) {
        println(s"[secondary] seq > currentSeq. ignored. seq: $seq, currentSeq: $currentSeq")
      }
      else if (seq < currentSeq) {
        println(s"[secondary] seq < currentSeq. seq: $seq, currentSeq: $currentSeq")
        println(s"[secondary] send SnapshotAck($key, seq = $seq)")
        sender ! SnapshotAck(key, seq)
      }
      else {
        if (valueOp.isDefined) kv += key -> valueOp.get
        else kv -= key
        currentSeq += 1
        println(s"[secondary] currentSeq is incremented to $currentSeq")
        replicators += sender
        val cancellableForPersist = context.system.scheduler.schedule(0 millisecond, 100 millisecond) {
          println(s"[secondary] send Persist($key, $valueOp, id = $seq)")
          persister ! Persist(key, valueOp, seq)
        }
        cancellableMapForPersist += seq -> cancellableForPersist
      }
    case Persisted(key, id) =>
      println(s"[secondary] received Persisted($key, id = $id)")
      cancellableMapForPersist(id).cancel()
      cancellableMapForPersist -= id
      replicators foreach {
        println(s"[secondary] send SnapshotAck($key, id = $id)")
        _ ! SnapshotAck(key, id)
      }
      // todo: is it ok to clear?
      replicators = Set.empty[ActorRef]
    case _ =>
  }

}

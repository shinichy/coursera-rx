package kvstore

import akka.actor.{Props, Actor}
import scala.util.Random
import java.util.concurrent.atomic.AtomicInteger

object Persistence {
  case class Persist(key: String, valueOption: Option[String], id: Long)
  case class Persisted(key: String, id: Long)

  class PersistenceException extends Exception("Persistence failure")

  def props(flaky: Boolean): Props = Props(classOf[Persistence], flaky)
}

class Persistence(flaky: Boolean) extends Actor {
  import Persistence._

  def receive = {
    case Persist(key, _, id) =>
      val nextBoolean = Random.nextBoolean()
//      println(s"[replicator] received Persist. key:$key, id:$id, nextBoolean:$nextBoolean")
      if (!flaky || nextBoolean) {
//        println(s"[replicator] send Persisted. key: $key, id: $id")
        sender ! Persisted(key, id)
      }
      else throw new PersistenceException
  }

}

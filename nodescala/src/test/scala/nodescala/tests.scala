package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("all has value") {
    val all: Future[scala.List[Int]] = Future.all(List(Future{1}, Future{2}, Future{3}))
    assert(Await.result(all, 1 second) == List(1, 2, 3))
  }

  test("all doesn't have value") {
    val all = Future.all(List(Future{1}, Future{throw new Exception}, Future{3}))
    try {
      Await.result(all, 1 second)
      fail()
    }
    catch {
      case _: Throwable =>
    }
  }

  test("any") {
    val any: Future[Int] = Future.any(List(Future{1}, Future{throw new Exception}, Future{3}))
    try {
      val result: Int = Await.result(any, 1 second)
      assert(Set(1, 3).contains(result))
    }
    catch {
      case _: Throwable =>
    }
  }

  test("delay") {
    val delay = Future.delay(1 second)
    Await.result(delay, 10 second)
  }

  test("now") {
    val p = Promise[Int]()
    try {
      p.future.now
      fail()
    } catch {
      case e: NoSuchElementException =>
      case _: Throwable => fail()
    }
    p.success(1)
    assert(p.future.now == 1)
  }

  test("continueWith") {
    val f1: Future[Int] = Future { throw new Exception}
    val f2 = Future { 2 }
    val cont = (f: Future[Int]) => f.now + 1
    try {
      Await.result(f1.continueWith(cont), 1 second)
      fail()
    }
    catch {
      case t: Throwable =>
    }

    assert(Await.result(f2.continueWith(cont), 1 second) == 3)
  }

  test("continueWith when cont throws Exception") {
    val f2 = Future { 2 }
    val cont = (f: Future[Int]) => throw new Exception

    try {
      assert(Await.result(f2.continueWith(cont), 1 second) == 3)
      fail()
    }
    catch {
      case t: Throwable =>
    }
  }

  test("continue") {
    val f: Future[Int] = Future { throw new Exception }
    val f2 = Future { 1 }
    val cont = (t: Try[Int]) => t match {
      case Success(x) => x + 1
      case Failure(e) => 0
    }

    assert(Await.result(f.continue(cont), 1 second) == 0)
    assert(Await.result(f2.continue(cont), 1 second) == 2)
  }

  test("continue when cont throws Exception") {
    val f2 = Future { 1 }
    val cont = (t: Try[Int]) => throw new Exception

    try {
      assert(Await.result(f2.continue(cont), 1 second) == 2)
      fail()
    }
    catch {
      case t: Throwable =>
    }
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

//  test("run") {
//    val working = Future.run() { ct =>
//      Future {
//        while (ct.nonCancelled) {
//          println("working")
//        }
//        println("done")
//      }
//    }
//    Future.delay(5 seconds) onSuccess {
//      case _ => working.unsubscribe()
//    }
//  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}





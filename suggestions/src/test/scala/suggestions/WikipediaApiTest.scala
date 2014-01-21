package suggestions


import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import suggestions.observablex.ObservableEx
import scala.collection.mutable
import rx.lang.scala.subscriptions.Subscription


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }

    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("ObservableEx.apply") {
    val list = Seq(1, 2, 3)
    val future: Future[Seq[Int]] = Future {
      list
    }

    val values = ObservableEx(future)
    values.subscribe {
      seq =>
        assert(seq == list)
    }
  }

//  test("ObservableEx.apply fails") {
//    val list = Seq(1, 2, 3)
//    val future: Future[Seq[Int]] = Future {
//      throw new Error("test error")
//    }
//
//    val values = ObservableEx(future)
//    try {
//      values.subscribe {
//        seq =>
//          fail()
//          assert(seq == list)
//      }
//    }
//    catch {
//      case t: Throwable =>
//    }
//  }

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("recovered") {
    val error = new Exception()
    val requests: Observable[Int] = Observable(observer => {
      observer.onNext(1)
      observer.onNext(2)
      observer.onNext(3)
      observer.onError(error)
      Subscription {}
    }
    )
    val observed = mutable.Buffer[Try[Int]]()
    requests.recovered.subscribe { i =>
      observed += i
    }

    assert(observed == Seq(Success(1), Success(2), Success(3), Failure(error)), observed)
  }

//  test("timedOut") {
//    val ticks: Observable[Long] = Observable.interval(1 second).take(5)
//
//    val observed = mutable.Buffer[Long]()
//    ticks.timedOut(3) subscribe {
//      observed += _
//    }
//    blocking {
//      Thread.sleep(3000)
//    }
//
//    assert(observed == Seq(0, 1), observed)
//  }

//  test("Correctly compose the streams that have errors using concatRecovered") {
//    val requests = Observable.interval(0.25.second).timedOut(1)
//    val exception = new Exception("test")
//    val remoteComputation = (num: Long) => if (num != 2) Observable(num) else Observable(exception)
//    val responses = requests.concatRecovered(remoteComputation)
//    val actual = responses.toBlockingObservable.toList
//    val expected = List(Success(0), Success(1), Failure(exception), Success(3))
//    assert(actual === expected, s"actual : $actual is not as expected : $expected")
//  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) {
      (acc, tn) =>
        tn match {
          case Success(n) => acc + n
          case Failure(t) => throw t
        }
    }
    var total = -1
    val sub = sum.subscribe { s =>
      println("s: " + s)
      total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }
}
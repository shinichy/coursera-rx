package suggestions
package gui

import scala.language.postfixOps
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Try, Success, Failure }
import rx.subscriptions.CompositeSubscription
import rx.lang.scala.{Observer, Observable}
import observablex._
import search._
import rx.lang.scala.Notification.{OnCompleted, OnError, OnNext}
import rx.lang.scala.subjects.ReplaySubject
import java.util.Calendar

trait WikipediaApi {

  /** Returns a `Future` with a list of possible completions for a search `term`.
   */
  def wikipediaSuggestion(term: String): Future[List[String]]

  /** Returns a `Future` with the contents of the Wikipedia page for the given search `term`.
   */
  def wikipediaPage(term: String): Future[String]

  /** Returns an `Observable` with a list of possible completions for a search `term`.
   */
  def wikiSuggestResponseStream(term: String): Observable[List[String]] = ObservableEx(wikipediaSuggestion(term))

  /** Returns an `Observable` with the contents of the Wikipedia page for the given search `term`.
   */
  def wikiPageResponseStream(term: String): Observable[String] = ObservableEx(wikipediaPage(term))

  implicit class StringObservableOps(obs: Observable[String]) {

    /** Given a stream of search terms, returns a stream of search terms with spaces replaced by underscores.
     *
     * E.g. `"erik", "erik meijer", "martin` should become `"erik", "erik_meijer", "martin"`
     */
    def sanitized: Observable[String] = {
      obs.map(_.replace(' ', '_'))
    }

  }

  implicit class ObservableOps[T](obs: Observable[T]) {

    /** Given an observable that can possibly be completed with an error, returns a new observable
     * with the same values wrapped into `Success` and the potential error wrapped into `Failure`.
     *
     * E.g. `1, 2, 3, !Exception!` should become `Success(1), Success(2), Success(3), Failure(Exception), !TerminateStream!`
     */
    def recovered: Observable[Try[T]] = {
//      obs.materialize.map {
//        case OnNext(v) => Success(v)
//        case OnError(e) => Failure(e)
//        case OnCompleted => {}
//      }

      Observable[Try[T]]((observer: Observer[Try[T]]) => {
        obs.materialize.subscribe(
          {
            case OnNext(n) => observer.onNext(Success(n))
            case OnError(e) => observer.onNext(Failure(e))
            case OnCompleted(_) => observer.onCompleted()
          },
          e => observer.onNext(Failure(e)),
          () => observer.onCompleted()
        )
      })
    }

    /** Emits the events from the `obs` observable, until `totalSec` seconds have elapsed.
     *
     * After `totalSec` seconds, if `obs` is not yet completed, the result observable becomes completed.
     *
     * Note: uses the existing combinators on observables.
     */
    def timedOut(totalSec: Long): Observable[T] = {
      Observable((observer: Observer[T]) => {
        val now = Calendar.getInstance().getTimeInMillis()
        println("now: " + now)
        obs.timestamp.subscribe (
          v => {
            println(v._1)
            if (v._1 - now > totalSec * 1000)
              observer.onCompleted()
            else
              observer.onNext(v._2)
          },
          e => observer.onError(e),
          () => observer.onCompleted()
        )
      })
    }


    /** Given a stream of events `obs` and a method `requestMethod` to map a request `T` into
     * a stream of responses `S`, returns a stream of all the responses wrapped into a `Try`.
     * The elements of the response stream should reflect the order of their corresponding events in `obs`.
     *
     * E.g. given a request stream:
     *
     * 1, 2, 3, 4, 5
     *
     * And a request method:
     *
     * num => if (num != 4) Observable.just(num) else Observable.error(new Exception)
     *
     * We should, for example, get:
     *
     * Success(1), Success(2), Success(3), Failure(new Exception), Success(5)
     *
     *
     * Similarly:
     *
     * Observable(1, 2, 3).concatRecovered(num => Observable(num, num, num))
     *
     * should return:
     *
     * Observable(Success(1), Succeess(1), Succeess(1), Succeess(2), Succeess(2), Succeess(2), Succeess(3), Succeess(3), Succeess(3))
     */
    def concatRecovered[S](requestMethod: T => Observable[S]): Observable[Try[S]] = {
      obs.map( t => {
        requestMethod(t).recovered
      }).concat
    }

  }

}


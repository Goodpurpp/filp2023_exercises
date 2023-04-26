package lection09

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Await

object Futures06 extends App {
  def createFuture(x: Int): Future[Int] = Future {
    Thread.sleep(x)
    x
  }

  val listOfFutures: List[Future[Int]] = List(createFuture(1), createFuture(2), createFuture(3))

  val futureOfList: Future[List[Int]] = Future.sequence(listOfFutures)

  val list = List(1,2,3)

  val futureOfList2: Future[List[Int]] = Future.traverse(list) { el => createFuture(el)}

  val failedFeature = Future.sequence(
    List(createFuture(1), createFuture(2), Future.failed(new Exception("!!1!")), createFuture(4))
  )

  Await.ready(futureOfList, 10.seconds)
  Await.ready(futureOfList2, 10.seconds)
  Await.ready(failedFeature, 10.seconds)

  println(futureOfList)
  println(futureOfList2)
  println(failedFeature)

}

object FuturesWithCats extends App {
  cats.instances.future.catsStdInstancesForFuture
}
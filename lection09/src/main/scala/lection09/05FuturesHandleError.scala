package lection09

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

// Рассказать про рекаверы
object FuturesErrors extends App {
  val failedFuture = Future {
    throw new Exception("AAAAA")
  }

  Await.ready(failedFuture, 10.seconds)

  println(failedFuture)

  //failed такой же блокирующий как successful
  val failedFuture1 = Future.failed(new Exception("BBBBB"))

  println(failedFuture1)
}

object FuturesRecovers extends App {
  val failedFuture = Future[Int] {
    throw new Exception("AAAAA")
  }

  val recoveredFuture = failedFuture.recover {
    case _: Exception => 42
  }

  val recoveredFuture2 = failedFuture.recoverWith {
    case _: Exception => Future {
      Thread.sleep(1000)
      43
    }
  }

  println(Await.result(recoveredFuture, 10.seconds))
  println(Await.result(recoveredFuture2, 10.seconds))

  val transformed = failedFuture.transform(
    x => x * 2,
    thr => new Exception("Bbbbb!!11")
  )

  println(Await.ready(transformed, 10.seconds))

  val transformedWith = failedFuture.transformWith {
    case Success(value) => Future.successful(println(value))
    case Failure(err) => Future.successful(println(s"Error: $err"))
  }
}


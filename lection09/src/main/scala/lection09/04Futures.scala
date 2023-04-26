package lection09

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

// scala бывает не только на jvm, но и на js и native еще бывает
object Futures03 extends App {

  val a: Future[String] = Future {
    Thread.sleep(2000)
    println("Hello from 1 task")
    "Kek"
  }

  val b: Future[Int] = Future {
    Thread.sleep(4000)
    println("Hello from 2 task")
    42
  }

  val c: Future[Boolean] = Future {
    Thread.sleep(6000)
    println("Hello from 3 task")
    true
  }

  println("Текущие состояния фьюч")
  println(a)
  println(b)
  println(c)

  println("Ждем выполнения фьюч")
  Await.ready(a, 10.seconds)
  Await.ready(b, 10.seconds)
  Await.ready(c, 10.seconds)

  println(a)
  println(b)
  println(c)

}

object FuturesMap extends App {
  val initFuture = Future[Int] {
    Thread.sleep(1000)
    42
  }

  val resultFuture = initFuture.map(_ * 2)

  println(resultFuture)

  println(Await.result(resultFuture, 10.seconds))
}

object FuturesFlatMap extends App {
  val initFuture = Future {
    Thread.sleep(1000)
    42
  }

  val resultFuture = initFuture.flatMap {prev =>
    Future {
      Thread.sleep(1000)
      prev * 2
    }
  }

  println(resultFuture)

  println(Await.result(resultFuture, 10.seconds))
}

object FuturesFor extends App {
  val initFuture = Future {
    Thread.sleep(1000)
    42
  }

  def hardTask(x: Int): Future[Int] = Future {
    Thread.sleep(1000)
    x * 2
  }

  val resultFuture = for {
    init <- initFuture
    x     = init + 3
    res  <- hardTask(x)
  } yield res

  println(resultFuture)

  println(Await.result(resultFuture, 10.seconds))
}

object FuturesSuccessful extends App {
  println("Создаем фьючу")
  val s = Future.successful {
    Thread.sleep(1000)
    println("Hello from successful future")
  }
  println("Создали фьючу, ее сообщение было выше")
}


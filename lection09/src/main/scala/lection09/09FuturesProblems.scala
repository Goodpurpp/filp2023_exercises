package lection09

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object FuturesProblems extends App {
  def initFuture = Future {
    Thread.sleep(1000)
    42
  }
  def a(x: Int) = Future {
    Thread.sleep(1000)
    x + 1
  }

  def b(x: Int) = Future {
    Thread.sleep(1000)
    x - 1
  }

  //Фьючи запускаются последовательно, хотя их можно стартовать и одновременно
  val res1 = for {
    init <- initFuture
    _a <- a(init)
    _b <- b(init)
  } yield _a + _b

  // Есть такой обходной путь, но он многословен

  val res2 = for {
    init <- initFuture
    runA = a(init)
    runB = b(init)
    _a    <- runA
    _b    <- runB
  } yield _a + _b

  // Переделать на флатмапе
  val failedFuture: Int => Future[Int] = _ => Future{ throw new Exception("Error")}
  val pure: Int => Future[Int] = x => Future { x }

  val composition1: Int => Future[Int] = failedFuture(_).flatMap(pure)
  val composition2: Int => Future[Int] = pure(_).flatMap(failedFuture)

  println(composition1(1))
  println(composition2(1))

  //===========================================================+
  //= Нельзя отменить фьючи, нельзя сделать race между фьючами |
  //===========================================================+
}

object FutureWithCats extends App {
  import cats.syntax.all._
  import cats.instances.all._

  def f(x: Int) = Future {
    Thread.sleep(5000)
    x * 2
  }

  val m = (f(1), f(2), f(3)).mapN(_ + _ + _)
  Await.result(m, 10.seconds)
  println(m)

  val seq: Future[List[Int]] = List(f(1), f(2), f(3)).sequence
  Await.result(seq, 10.seconds)
  println(seq)

  val trav: Future[List[Int]] = List(1, 2, 3).traverse(f(_))
  Await.result(trav, 17.seconds)
  println(trav)

}
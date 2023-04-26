package lection09
import cats.effect._

object IOLaws extends App {
  val pure: Int => IO[Int] = IO[Int](_)
  val failed: Int => IO[Int] = _ => IO(throw new Exception("ATTENTION"))

  val io1: Int => IO[Int] = pure(_).flatMap(failed)
  val io2: Int => IO[Int] = failed(_).flatMap(pure)

  println(io1(1))
  println(io2(1))
}
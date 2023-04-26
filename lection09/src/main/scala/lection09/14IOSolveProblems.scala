package lection09
import cats.effect.{ExitCode, IO}
import cats.syntax.all._
import scala.concurrent.duration._

object IOSolveProblemsPar extends cats.effect.IOApp {
  val io1 = IO.sleep(5.seconds) >> IO.pure(42)
  val io2 = IO.sleep(5.seconds) >> IO.pure(34)

  override def run(args: List[String]): IO[ExitCode] = for {
    initTime <- IO.monotonic
    i1r      <- io1.start
    i2r      <- io2.start
    i1j      <- i1r.joinWith(IO.pure(0))
    i2j      <- i2r.joinWith(IO.pure(0))
    _        <- IO.println(s"Result: ${i1j + i2j}")
    time1    <- IO.monotonic
    _        <- IO.println(s"Time: ${(time1 - initTime).toSeconds}")

    res      <- (io1, io2).parMapN(_ + _)
    time3    <- IO.monotonic
    _        <- IO.println(s"Result: $res")
    _        <- IO.println(s"Time: ${(time3 - time1).toSeconds}")
  } yield ExitCode.Success
}

object IOSolveProblemsCanc extends cats.effect.IOApp {

  def recIO(msg: String): IO[Unit] = IO.sleep(2.seconds) >> IO.println(msg) >> recIO(msg)

  override def run(args: List[String]): IO[ExitCode] = for {
    task <- recIO("Hello").start
    _    <- IO.sleep(10.seconds)
    _    <- task.cancel
    _    <- IO.println("cancel")
    _    <- IO.sleep(10.seconds)
    _    <- IO.println("Goodbye")
  } yield ExitCode.Success
}

object IORace extends cats.effect.IOApp {

  val io1 = IO.sleep(3.seconds) >> IO.println("IO1")
  val io2 = IO.delay(2.seconds) >> IO.println("IO2")

  override def run(args: List[String]): IO[ExitCode] =
    IO.race(io1, io2) >> IO.sleep(4.seconds).as(ExitCode.Success)
}
package lection09
import cats.effect.{ExitCode, IO}

object IOErrors extends cats.effect.IOApp {
  val iofailed = IO.raiseError[Int](
    new Exception("exception")
  )
  override def run(args: List[String]): IO[ExitCode] =
    iofailed
      .recover {
        case _ => 34
      }
      .flatMap { x =>
        IO.println(x) >> IO.raiseError(new Exception("new Exception"))
      }
      .recoverWith {
        case _ => IO.println("recoverWith") >> IO.pure(11)
      }
    .as(ExitCode.Success)
}
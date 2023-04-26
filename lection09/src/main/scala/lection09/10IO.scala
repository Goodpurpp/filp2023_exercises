package lection09
import cats.effect.{ExitCode, IO}

object IOApp extends cats.effect.IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    IO.delay(println("42")).as(ExitCode.Success)

}

object Kek extends App {
  import scala.concurrent.{Await, Future}
  import scala.concurrent.duration._

  val rt = cats.effect.unsafe.IORuntime.builder().build()

  println("Hello")
  IO.delay(println("42")).unsafeRunSync()(rt)

  val futureFromIO: Future[String] = IO.delay("34").unsafeToFuture()(rt)

  println(Await.result(futureFromIO, 10.seconds))

  val asyncFromIO: (Either[Throwable, String] => Unit) => Unit = IO.delay("lol").unsafeRunAsync(_)(rt)

  asyncFromIO(println)

  Thread.sleep(100)
}
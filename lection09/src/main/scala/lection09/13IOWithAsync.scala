package lection09
import cats.effect.{ExitCode, IO}

object IOWithAsync extends cats.effect.IOApp {
  /*
   * Для того, чтобы запускать IO на другом ExecutionContext'е, как
   * это было с фьючам, потребуется много кода, который больше засорит пример.
   */
  val tp   = new MyCoolThreadPool
  val usio = new UserServiceIOWrapper(new UserService.Stub(tp))
  val csio = new CarServiceIOWrapper(new CarService.Stub(tp))

  override def run(args: List[String]): IO[ExitCode] = for {
    user <- usio.getUserByPhone(1234)
    nums <- csio.getCarNumbers(user.passport)
    _    <- IO.println(nums)
  } yield ExitCode.Success
}

class UserServiceIOWrapper(us: UserService) extends UserServiceIO {
  override def getUserByPhone(phone: Long): IO[UserService.User] =
    IO.async_(cb => us.getUserByPhone(phone)(x => cb(x.toEither)))
}

class CarServiceIOWrapper(cs: CarService) extends CarServiceIO {
  override def getCarNumbers(passport: UserService.Passport): IO[List[CarServiceFuture.CarNumber]] =
    IO.async_(cb => cs.getNumbersByPassport(passport)(cb.compose(_.toEither)))
}
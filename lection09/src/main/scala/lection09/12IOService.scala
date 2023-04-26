package lection09
import cats.effect.{ExitCode, IO}
import lection09.CarServiceFuture.CarNumber
import lection09.UserService.{User, Passport}

object IOService extends cats.effect.IOApp {
  val userService = UserServiceIO.Stub
  val carService = CarServiceIO.Stub

  override def run(args: List[String]): IO[ExitCode] = for {
    user <- userService.getUserByPhone(1234)
    nums <- carService.getCarNumbers(user.passport)
    _    <- IO.println(nums)
  } yield ExitCode.Success

}

trait UserServiceIO {
  def getUserByPhone(phone: Long): IO[User]
}
trait CarServiceIO {
  def getCarNumbers(passport: Passport): IO[List[CarNumber]]
}

object UserServiceIO {
  object Stub extends UserServiceIO {
    override def getUserByPhone(phone: Long): IO[User] = {
      IO.delay {
        Thread.sleep(1000)
        User("Ivan", Passport(1234, 567890))
      }
    }
  }
}

object CarServiceIO {
  object Stub extends CarServiceIO {
    override def getCarNumbers(passport: Passport): IO[List[CarNumber]] = {
      IO.delay {
        Thread.sleep(1000)
        List(CarNumber("KE333K"))
      }
    }
  }
}
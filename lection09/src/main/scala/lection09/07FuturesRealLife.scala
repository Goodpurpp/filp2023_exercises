package lection09

import lection09.CarServiceFuture.CarNumber
import lection09.UserService.{Passport, User}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

object MyApp extends App {
  val userService: UserServiceFuture = UserServiceFuture.Stub
  val carService: CarServiceFuture = CarServiceFuture.Stub

  val phone = 79123456789L

  val carNumbers = for {
    userInfo <- userService.getUserByPhone(phone)
    passport  = userInfo.passport
    nums     <- carService.getNumbersByPassport(passport)
  } yield nums

  println(Await.result(carNumbers, 10.seconds))
}

trait UserServiceFuture {
  def getUserByPhone(phone: Long): Future[User]
}

object UserServiceFuture {

  object Stub extends UserServiceFuture {
    override def getUserByPhone(phone: Long): Future[User] =
      Future {
        Thread.sleep(1000)
        User("Иван", Passport(1234, 567890))
      }
  }
}

trait CarServiceFuture {
  def getNumbersByPassport(p: Passport): Future[List[CarNumber]]
}

object CarServiceFuture {
  case class CarNumber(value: String) extends AnyVal

  object Stub extends CarServiceFuture {
    override def getNumbersByPassport(p: Passport): Future[List[CarNumber]] =
      Future {
        Thread.sleep(1000)
        Nil
      }
  }
}
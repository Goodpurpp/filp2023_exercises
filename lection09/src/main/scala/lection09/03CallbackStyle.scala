package lection09

import lection09.CarServiceFuture.CarNumber
import lection09.UserService.Passport

import scala.util.{Success, Try}

object CallbackStyle extends App {
  val tp = new MyCoolThreadPool
  val userService = new UserService.Stub(tp)
  val carService  = new CarService.Stub(tp)

  userService.getUserByPhone(1234) {
    case Success(value) =>
      carService.getNumbersByPassport(value.passport) {
        case Success(value) => println(value)
        case _ => println("CarService: Oh no")
      }
    case _ => println("UserService: Oh no")
  }

  println("Ждем, пока случится вывод номера машины")
  Thread.sleep(2100)
  println("Номер машины получили выше")
}

// Коллбек - это функция, которая передается другой функции, и которая
// будет вызвана, по завершении той функции, куда она была предана
trait UserService {
  import UserService._
  def getUserByPhone(phone: Long)(cb: Try[User] => Unit): Unit
}

trait CarService {
  def getNumbersByPassport(p: Passport)(cb: Try[List[CarNumber]] => Unit): Unit
}

object UserService {
  case class User(name: String, passport: Passport)
  case class Passport(serial: Int, number: Int)

  class Stub(tp: MyCoolThreadPool) extends UserService {
    override def getUserByPhone(phone: Long)(cb: Try[User] => Unit): Unit = {
      tp.run{ () =>
        Thread.sleep(1000)
        cb(scala.util.Success(User("Ivan", Passport(1234, 567890))))
      }
    }
  }
}



object CarService{
  class Stub(tp: MyCoolThreadPool) extends CarService {
    override def getNumbersByPassport(p: Passport)(cb: Try[List[CarNumber]] => Unit): Unit = {
      tp.run { () =>
        Thread.sleep(1000)
        cb(scala.util.Success(List(CarNumber("XH102P"))))
      }
    }
  }
}

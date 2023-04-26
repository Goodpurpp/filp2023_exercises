package lection09
import lection09.CarServiceFuture.CarNumber
import lection09.UserService.{Passport, User}

import java.util.concurrent.Executor
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import scala.concurrent.Await

object FutureCallbacks extends App {
  //Импортировать глобальный контекст - не оч, в зависимости от задач
  //лучше задавать свои
  val tp = new MyCoolThreadPool with Executor {
    override def execute(command: Runnable): Unit = this.run(() => command.run())
  }
  implicit val executionContext = ExecutionContext.fromExecutor(tp)

  val usw = new UserServiceWrapper(new UserService.Stub(tp))
  val csw = new CarServiceWrapper(new CarService.Stub(tp))

  val res = for {
    u <- usw.getUserByPhone(1234)
    n <- csw.getNumbersByPassport(u.passport)
  } yield n

  println(Await.result(res, 10.seconds))
}

class UserServiceWrapper(userService: UserService) extends UserServiceFuture {
  override def getUserByPhone(phone: Long): Future[User] = {
    val promise = Promise[User]
    userService.getUserByPhone(phone)(promise.complete)
    promise.future
  }
}

class CarServiceWrapper(carService: CarService) extends CarServiceFuture {
  def getNumbersByPassport(p: Passport): Future[List[CarNumber]] = {
    val promise = Promise[List[CarNumber]]
    carService.getNumbersByPassport(p)(promise.complete)
    promise.future
  }
}
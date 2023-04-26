package lection09

object Thread_ extends App {

  val a = new Thread {
    override def run(): Unit = {
      Thread.sleep(2000)
      println("1. Hello from thread 1")
    }
  }

  val b = new Thread {
    override def run() = {
      Thread.sleep(4000)
      println("2. Hello from thread 2")
    }
  }

  println("Сейчас мы тут")
  a.start()
  b.start()
  println("Теперь мы запустили треды 'a' и 'b'")

  println("А вот теперь мы заблокируемся и дождемся, пока треды выполнят свою работу")
  val startTime = System.currentTimeMillis()
  println(s"ElapsedTime: 0")
  a.join()
  println("Тред 'a' закончил работу")
  println(s"ElapsedTime: ${System.currentTimeMillis() - startTime}")
  b.join()
  println("Тред 'b' закончил работу")
  println(s"ElapsedTime: ${System.currentTimeMillis() - startTime}")

}
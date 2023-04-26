package lection09

import java.util.concurrent.LinkedBlockingQueue

object ThreadPool extends App {

  val threadPool = new MyCoolThreadPool

  // слипы, многоядерность и конкурентность
  threadPool.run(() => {
    Thread.sleep(2000)
    println("Hello from 1 task")
  })

  threadPool.run(() => {
    Thread.sleep(4000)
    println("Hello from 2 task")
  })

  threadPool.run(() => {
    Thread.sleep(6000)
    println("Hello from 3 task")
  })

  println("Мы запустили таски и теперь ждем, когда они закончат работу")

  Thread.sleep(6100)

  println("Все таски завершили работу")
}

trait TP {
  def run(task: () => Unit): Unit
}

class MyCoolThreadPool extends TP {
  private val queue = new LinkedBlockingQueue[() => Unit](10)

  val threads = List.fill(4)(0).map {_ =>
    new Thread {
      override def run(): Unit =
        while(true) {
          val task = queue.poll()
          if (task == null) ()
          else task()
        }
    }
  }

  threads.foreach(_.setDaemon(true))
  threads.foreach(_.start())

  override def run(task: () => Unit): Unit =
    queue.offer(task)
}
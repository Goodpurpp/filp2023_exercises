package exercises03

import scala.annotation.tailrec

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {
  def sum(list: MyList[Int]): Int = {
    @tailrec
    def summaryList(a: MyList[Int], acc: Int = 0): Int = a match {
      case Nil                                => acc
      case Cons(head: Int, tail: MyList[Int]) => summaryList(tail, acc + head)
    }
    summaryList(list)
  }

  def reverse[A](list: MyList[A]): MyList[A] = {
    @tailrec
    def reversedList[A](a: MyList[A], acc: MyList[A]): MyList[A] = a match {
      case Nil                         => acc
      case Cons(head, tail: MyList[A]) => reversedList(tail, Cons(head, acc))
    }
    reversedList(list, Nil)
  }

}

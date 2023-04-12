package exercises06.e4_eq
import EqSyntax._

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}

object EqInstances {
  implicit val booleanEq: Eq[Boolean] = _ == _
  implicit val intEq: Eq[Int]         = _ == _

  implicit def listEq[A: Eq]: Eq[List[A]] = (a, b) => {
    if (a.length != b.length) false
    else a.zip(b).forall { case (x, y) => x.eqv(y) }
  }

  implicit def optionEq[A: Eq]: Eq[Option[A]] =
    (x, y) =>
      (x, y) match {
        case (Some(a), Some(b)) => a.eqv(b)
        case (None, None)       => true
        case _                  => false
      }
}

object EqSyntax {
  implicit class EqOps[A](val a: A) extends AnyVal {
    def eqv(b: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, b)
    def ===(b: A)(implicit ev: Eq[A]): Boolean = eqv(b)
    def !==(b: A)(implicit ev: Eq[A]): Boolean = !eqv(b)
  }
}

object Examples {
  import EqInstances._
  import EqSyntax._

  1 eqv 1 // возвращает true
  1 === 2 // возвращает false
  1 !== 2 // возвращает true
  //   1 === "some-string" // не компилируется
  //   1 !== Some(2) // не компилируется
  List(true) === List(true) // возвращает true
}

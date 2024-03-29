package typeclasses

import typeclasses.Example01Monoids.Semigroup
import typeclasses.Example03Applicatives._
import typeclasses.Example04MapN._
import typeclasses.Example08ApplicativeError.ApplicativeError

object Example09Validated extends App {
  sealed trait Validated[+E, +A]

  object Validated {
    case class Valid[+A](a: A) extends Validated[Nothing, A]

    case class Invalid[+E](e: E) extends Validated[E, Nothing]
  }

  implicit class RaiseErrorSyntax[E](private val e: E) extends AnyVal {
    def raiseError[F[_], A](implicit ae: ApplicativeError[F, E]): F[A] =
      ae.raiseError(e)
  }

  implicit def validatedApplicativeError[E: Semigroup]: ApplicativeError[Validated[E, *], E] =
    new ApplicativeError[Validated[E, *], E] {
      def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] =
        fa match {
          case Validated.Valid(fa)    => Validated.Valid(f(fa))
          case Validated.Invalid(err) => Validated.Invalid(err)
        }

      def pure[A](x: A): Validated[E, A] =
        Validated.Valid(x)

      def raiseError[A](e: E): Validated[E, A] =
        Validated.Invalid(e)

      def ap[A, B](ff: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] = {
        import Validated._

        (ff, fa) match {
          case (Valid(f), Valid(a))       => Valid(f(a))
          case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
          case (e @ Invalid(_), _)        => e
          case (_, e @ Invalid(_))        => e
        }
      }

      def handleErrorWith[A](fa: Validated[E, A])(f: E => Validated[E, A]): Validated[E, A] =
        fa match {
          case Validated.Invalid(e) => f(e)
          case other                => other
        }
    }

  case class NonEmptyList[+A](head: A, tail: List[A] = Nil)

  implicit def nelSemigroup[A]: Semigroup[NonEmptyList[A]] =
    (x, y) => NonEmptyList(x.head, x.tail ::: (y.head :: y.tail))

  type V[+A] = Validated[NonEmptyList[String], A]

  trait FromString[A] {
    def parse(str: String): V[A]
  }

  object FromString {
    implicit val idFromString: FromString[String] = Validated.Valid(_)

    implicit val intFromString: FromString[Int] = str =>
      str.toIntOption match {
        case Some(int) => int.pure[V]
        case None      => NonEmptyList(s"Can't parse int from $str").raiseError
      }

    implicit val doubleFromString: FromString[Double] = str =>
      str.toDoubleOption match {
        case Some(double) => double.pure[V]
        case None         => NonEmptyList(s"Can't parse double from $str").raiseError
      }
  }

  implicit class StringSyntax(private val str: String) extends AnyVal {
    def parse[A](implicit fromString: FromString[A]): V[A] =
      fromString.parse(str)
  }

  case class Person(name: String, age: Int, height: Double)

  val success: V[Person] = ("Ilya".parse[String], "22".parse[Int], "187.8".parse[Double]).mapN(Person)

  println(success) // Valid(Person(Ilya, 22, 187.8))

  val failed: V[Person] = ("Ilya".parse[String], "lol".parse[Int], "kek".parse[Double]).mapN(Person)

  println(failed) // Invalid(NonEmptyList(Can't parse int from lol, List(Can't parse double from kek)))
}

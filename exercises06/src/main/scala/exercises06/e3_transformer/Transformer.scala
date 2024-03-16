package exercises06.e3_transformer

import exercises06.e3_transformer.Error.{InvalidId, InvalidName}

trait Transformer[A, B] {
  def toOption(a: A): Option[B]

  def toEither(a: A): Either[Error, B]
}

object TransformerInstances {
  implicit val transformer: Transformer[RawUser, User] = new Transformer[RawUser, User] {
    def toOption(a: RawUser): Option[User] = toEither(a).toOption

    def toEither(a: RawUser): Either[Error, User] =
      for {
        id        <- a.id.toLongOption.toRight(InvalidId)
        firstName <- a.firstName.toRight(InvalidName)
        lastName  <- a.secondName.toRight(InvalidName)
      } yield User(id, UserName(firstName, lastName, a.thirdName))
  }
}

object TransformerSyntax {
  implicit class TransformerOps[A](a: A) {
    def transformToOption[B](implicit transformer: Transformer[A, B]): Option[B]        = transformer.toOption(a)
    def transformToEither[B](implicit transformer: Transformer[A, B]): Either[Error, B] = transformer.toEither(a)
  }
}

object Examples {
  import TransformerInstances._
  import TransformerSyntax._

  RawUser("1234", Some(""), Some(""), None).transformToOption[User]
  RawUser("1234", Some(""), Some(""), None).transformToEither[User]
}

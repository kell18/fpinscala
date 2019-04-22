package fpinscala.playground

import cats.effect.IO
import cats.effect.ExitCase.Error

object Cats extends App {

  sealed abstract class DbError extends Exception with Product with Serializable

  object DbError {
    final case object InvalidSql extends DbError
    final case object ConnectionLost extends DbError
  }

  import DbError._

  def connectDb: IO[Unit] = ???

  /*connectDb.bracketCase { _ =>
    IO.raiseError(ConnectionLost)
  } {
    case (r, Error(InvalidSql))     =>

    case (r, Error(ConnectionLost)) =>
  }*/

}

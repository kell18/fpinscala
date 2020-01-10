/*
package playground


import scala.concurrent.ExecutionContext
import cats.effect._
import fs2._
import fs2.concurrent._
import scala.language.higherKinds


object Fs2Sink {

  /*def queueFromAsyncCallback[T, F[_]](registerCalback: (Either[Throwable, T] => Unit))(
    implicit F: Async[F]
  ): Async[F] = {

  }*/

  type Row = List[String]

  trait CSVHandle {
    def withRows(cb: Either[Throwable, Row] => Unit): Unit
  }

  // .. split this code (mb queue creation, population and consumption):
  //   https://underscore.io/blog/posts/2018/03/20/fs2.html
  //   https://fs2.io/guide.html#asynchronous-effects-callbacks-invoked-multiple-times
  def rows[F[_]](maxInFlight: Int, h: CSVHandle)(implicit F: ConcurrentEffect[F], cs: ContextShift[F]): Stream[F,Row] =
    for {
      q <- Stream.eval(Queue.bounded[F, Either[Throwable, Row]](maxInFlight))
      _ <-  Stream.eval { F.delay(h.withRows(e => F.runAsync(q.enqueue1(e))(_ => IO.unit).unsafeRunSync)) }
      row <- q.dequeue.rethrow
    } yield row
}




object StreamingTests extends App {

}
*/

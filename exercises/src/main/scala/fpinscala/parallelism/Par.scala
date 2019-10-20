package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

// Some initial thoughts
/*trait Parr[A] {
  def map[B](f: A => B): Parr[B]
  def zip[B](x: Parr[B]): Parr[(A, B)]
}

case class Unit[T](v: T) extends Parr
case class Fork[T](v: Parr[T]) extends Parr

object Parr {
  def unit[T](v: T) = new Parr(v)

  def map2[A, B, C](a: Parr[A], b: Parr[B])(f: (A, B) => C) = a.zip(b).map { case (a, b) => f(a, b) }
}*/



object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  object FutureUtils {
    def getWithMillisSpent[A](f: Future[A])(timeout: Long, units: TimeUnit): (A, Double) = {
      val t1 = System.nanoTime
      val r = f.get(timeout, units)
      val millis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
      r -> millis
    }
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def parFoldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): Par[B] = {
    as.foldLeft(unit(acc)) {
      case (ac, a) => fork(unit(f(a, ac)))
    }
  }

  def paragraphsTotalWords(ps: List[String]): Par[Int] = parFoldLeft(ps)(0) {
    case (total, p) => total + p.split(" ").length
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val a = parMap(as)(a => List(a).filter(f))
    map(a)(_.flatten)
  }

  def parFilter_parFold[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val reversed = parFoldLeft(as)(List.empty[A]) {
      case (filtered, a) => if (f(a)) a :: filtered else filtered
    }
    map(reversed)(_.reverse)
  }

  def zip[A, B](a: Par[A], b: Par[B]): Par[(A, B)] = map2_(a, b)(_ -> _)

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    map2_(zip(a, b), c) { case ((a, b), c) => f(a, b, c) }
  }

  def sequence1[A](a: List[Par[A]]): Par[List[A]] = {
    val reversed = a.foldLeft(Par.unit(List.empty[A])) {
      case (pars, i) => map2_(pars, i) { case (x, y) => y :: x }
    }
    map(reversed)(_.reverse)
  }

  def sequence2[A](a: List[Par[A]]): Par[List[A]] =
    a.foldRight(Par.unit(List.empty[A])) {
      case (i, pars) => map2_(i, pars)(_ :: _)
    }


  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = sequence2(as.map(asyncF(f)))

  def map2_[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C) = map2(a, b)(f)(100, TimeUnit.SECONDS)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C)(timeout: Long, units: TimeUnit): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val (af, millisSpent) = FutureUtils.getWithMillisSpent(a(es))(timeout, units)
      val bf = b(es).get(millisSpent, TimeUnit.MILLISECONDS)
      // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures.
      // This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait.
      // It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and
      // `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new
      // `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from
      // the available time allocated for evaluating `bf`.
      UnitFuture(f(af, bf))
    }

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(a)

  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}

package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A]

  def drop(n: Int): Stream[A]

  def takeWhile(p: A => Boolean): Stream[A]

  def takeWhile_foldRight(p: A => Boolean): Stream[A]

  def forAll(p: A => Boolean): Boolean

  def headOption: Option[A]

  def toList: List[A]

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}

case object Empty extends Stream[Nothing] {
  override def take(n: Int): Stream[Nothing] = this

  override def drop(n: Int): Stream[Nothing] = this

  override def takeWhile(p: Nothing => Boolean): Stream[Nothing] = this

  override def forAll(p: Nothing => Boolean): Boolean = false

  override def headOption: Option[Nothing] = None

  override def toList = List.empty

  override def takeWhile_foldRight(p: Nothing => Boolean) = this
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def take(n: Int) = if (n > 0) {
    lazy val newTail = t().take(n-1)
    Cons(h, () => newTail)
  } else if (n == 0) {
    Empty
  } else {
    throw new RuntimeException(s"Cannot rake negative number of elements: $n")
  }

  override def drop(n: Int) = if (n > 0) t().drop(n - 1) else
    if (n == 0) this else
    throw new RuntimeException(s"Cannot drop negative number of elements: $n")

  override def takeWhile(p: A => Boolean) = if (p(h())) {
    lazy val newTail = t().takeWhile(p)
    Cons(h, () => newTail)
  } else Empty

  // .. ex. >= 5.6 (and reading there)
  def takeWhile_foldRight(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) {
    case (a, s) if p(a) => Stream.cons(a, s)
    case (_, s) => Stream.empty // not s!
  }

  override def forAll(p: A => Boolean) = if(!p(h())) false else t().forAll(p)

  override def headOption = Some(h())

  override def toList = h() :: t().toList
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[T](c: T): Stream[T] = Stream.cons(c, constant(c))

  def constant_unfold[T](c: T): Stream[T] = unfold(c)(c => Some(c -> c))

  def from(n: Int): Stream[Int] = Stream.cons(n, Stream(n + 1))

  def from_unfold(n: Int): Stream[Int] = unfold(n)(i => Some(i -> (i + 1)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Stream.empty
  }

  def fibs(): Stream[Long] = {
    def go(a: Long, b: Long): Stream[Long] = Stream.cons(a, go(b, a + b))
    go(0, 1)
  }

  def fibs_unfold(): Stream[Long] = {
    unfold(0L -> 1L) { case (a, b) => Some((a, b -> (a + b))) }
  }
}
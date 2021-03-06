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

  def headOption_foldRight: Option[A] =
    foldRight[Option[A]](None) {
      case (a, _) => Some(a)
    }


  // via unfold
  def map[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(head, tail) => Some(f(head()), tail())
    case _ => None
  }

  def take_unfold(n: Int): Stream[A] = unfold(this -> n) {
    case (Cons(head, tail), remains) if remains > 0 => Some(head(), (tail(), remains-1))
    case _ => None
  }

  def takeWhile_unfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(head, tail) if p(head()) => Some(head(), tail())
    case _ => None
  }


  def zipWith_unfold[B](strB: Stream[B]): Stream[(A, B)] = unfold(this -> strB) {
    case (Cons(headA, tailA), Cons(headB, tailB)) => Some(headA() -> headB(), tailA() -> tailB())
    case _ => None
  }


  def toList: List[A]


  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case nonEmptyStr => Some((nonEmptyStr, nonEmptyStr.drop(1)))
  }.append(Stream(empty))

  // stack-unsafe. Stack-safe could be probably done with recursion.
  def append[AA >: A](s: Stream[AA]): Stream[AA] = unfold(this -> s) {
    case (Empty, Cons(h, t)) => Some(h() -> (Empty, t()))
    case (Cons(h, t), rest) => Some(h() -> (t(), rest))
    case _ => None
  }

  def hasSubsequence[AA >: A](subSeq: Stream[AA]): Boolean = tails.exists(_.startsWith(subSeq))

  def zip2f[B, C](s: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = ???


  def startsWith[B](s: Stream[B]): Boolean = this.zip2f(s) {
    case (a, b) => true // todo mapN // .. from work-mac
  }.forAll(_ == true)

  def tailsViaScanRight: Stream[Stream[A]] = this.scanRight(this) {
    case (Cons(_, t), _) => t()
    case (_, _) => Empty
  }

  def scanRight[S](s: S)(f: (S, A) => S): Stream[S] = this match {
    case Cons(h, t) => {
      val newS = f(s, h())
      val scannedRight = t().scanRight(newS)(f)
      cons(newS, scannedRight)
    }
    case Empty => Empty
  }

  def scanRightViaUnfold[S](s: S)(f: (S, A) => S): Stream[S] = ??? // some kind of bullshit to do it... :D

  def scanRightViaFoldRight_ORIGINAL[S](s: S)(f: (A, => S) => S): Stream[S] = tails.map(_.foldRight(s)(f))

  def scanRightViaFoldRight[S](s: S)(f: (A, S) => S): Stream[S] = this.foldRight(s -> empty[S]) {
    case (a, (newS, interimResult)) => {
      val s1 = f(a, newS)
      newS -> cons(s1, interimResult)
    }
  }._2
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

  // .. move to the Stream itelf

  def zipAll_unfold[A, B](strA: Stream[A])(strB: Stream[B]): Stream[(Option[A],Option[B])] = ???

  def zip_unfold[A, B, C](f: (Option[A], Option[B]) => C)(strA: Stream[A])(strB: Stream[B]): Stream[C] =
    unfold(strA -> strB) {
      case (Empty, Empty) => None
      case (a, b) =>
        Some(
          f(a.headOption, b.headOption),
          a.drop(1) -> b.drop(1)
        )
    }
}
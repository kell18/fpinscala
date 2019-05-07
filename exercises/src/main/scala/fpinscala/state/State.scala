package fpinscala.state

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt1(rng: RNG): (Int, RNG) = {
    val (unbounded, nextRng) = rng.nextInt
    val bounded = Math.max(unbounded, Int.MinValue + 1)
    (Math.abs(bounded), nextRng)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (unbounded, nextRng) = rng.nextInt
    (if (unbounded < 0) -(unbounded + 1) else unbounded) -> nextRng
  }

  def double(rng: RNG): (Double, RNG) = {
    val (posInt, nextRng) = nonNegativeInt(rng)
    (posInt / Int.MaxValue.toDouble + 1.0) -> nextRng
  }

  def double_map(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1.0)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, rng1) = rng.nextInt
    val (dbl, rng2) = double(rng1)
    (int, dbl) -> rng2
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (intDbl, nextRng) = intDouble(rng)
    intDbl.swap -> nextRng
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    (d1, d2, d3) -> rng3
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (firstI, secondRng) = rng.nextInt
    val intsAndRng = (0 until count).scanLeft(firstI -> secondRng) {
      case ((_, nextRng), _) => nextRng.nextInt
    }
    intsAndRng.map(_._1).toList -> intsAndRng.lastOption.map(_._2).getOrElse(secondRng)
  }

  def ints_recursive(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(c: Int, l: List[Int], r: RNG): (List[Int], RNG) = {
      if (c < 1) {
        (l, r)
      }
      else {
        val (nextI, nextR) = r.nextInt
        go(c - 1, nextI :: l, nextR)
      }
    }

    go(count, List.empty[Int], rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rnd => {
      val (a, newRa) = ra(rnd)
      val (b, newRb) = rb(newRa)
      f(a, b) -> newRb
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      val (firstI, nextRng) = fs.head(rng)
      val b = fs.drop(1).scanLeft(fs.head(rng)) { case ((aI, rngI), rand) => rand(rngI) }
      b.map(_._1) -> b.last._2
    }
  }

  def sequence_rec[A](a: List[Rand[A]]): Rand[List[A]] = firstRng => {
    @tailrec
    def go(ax: List[Rand[A]], sequenced: List[A], prevRng: RNG): (List[A], RNG) = ax match {
      case x :: xs => {
        val (i, rng) = x(prevRng)
        go(xs, i :: sequenced, rng)
      }
      case Nil     =>
        sequenced -> prevRng
    }

    go(a, Nil, firstRng)
  }

  def emptyRand[A](v: A): Rand[A] = rnd => (v, rnd)

  /*def sequence_foldr[A](a: List[Rand[A]]): Rand[List[A]] =
    a.foldRight(emptyRand(Nil[A])) { case (b, c) =>
      map2(b, c)(_ :: _) // Something like that
    }*/


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = firstRng => {
    val (fI, fRng) = f(firstRng)
    g(fI)(fRng)
  }

  def map_fmap[A,B](f: Rand[A])(g: A => B): Rand[B] = flatMap(f) { a =>
    emptyRand(g(a))
  }

  def map2_fmap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a, b)
      }
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State { s =>
      val (a, newS) = run(s)
      f(a) -> newS
    }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State { s =>
      val (baseA, baseS) = run(s)
      val (argA, argS) = sb.run(baseS)
      f(baseA, argA) -> argS
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (baseA, baseS) = run(s)
      f(baseA).run(baseS)
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

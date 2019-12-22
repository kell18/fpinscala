package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  self =>

  def check: Boolean

  def AND(b: Prop): Prop = new Prop {
    def check = self.check && b.check
  }

  def AND1(b: Prop): Prop = if (check) b else this
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A, B](f: A => B): Gen[B] = ???

  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

object SomeChecks {
  // 8.1, 8.2:
  // List.Sum prop:
  //  - sum(l) == sum(shuffle(l))
  //  - sum(l) - sum(l) == 0
  //  - sum(l) - sum(Nil) == sum(l)
  //  - forAnySublist(l) { sum(subL) <= sum(l)
  //  - sum(l) >= max(l)

  // List.Max props:
  //  - max(l) == max(shuffle(l))
  //  - sum(l) >= max(l)
  //  - l.forall(_ <= max(l))
  //  - max(l) >= max(l.tail) given l.nonEmpty
}


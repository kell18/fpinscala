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

object SomeChecks extends App {

  val r = Gen2.boolean.sample.run(RNG.Simple(63453))
  println(r)

  val r1 = Gen2.listOfN(7, Gen2.boolean)
  println(r1.sample.run(RNG.Simple(4L)))


  case class Gen2[A](sample: State[RNG,A])

  object Gen2 {
    def apply[A](run: RNG => (A, RNG)): Gen2[A] = Gen2[A](State(run))


    def unit[A](a: => A): Gen2[A] = Gen2(a -> _)

    def boolean: Gen2[Boolean] = Gen2(RNG.boolean(_))

    def listOfN[A](n: Int, g: Gen2[A]): Gen2[List[A]] = Gen2[List[A]] { firstR: RNG =>
      val (initI, initR) = g.sample.run(firstR)
      (0 until n).foldLeft(List(initI) -> initR) {
        case ((is, r), _) =>
          val (nextI, nextR) = g.sample.run(r)
          (nextI :: is, nextR)
      }
    }

    // def choose(start: Int, stopExclusive: Int): Gen2[Int] = Gen2(State(rng => rng.nextInt))
  }


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


package my.cats

import java.lang.Number

import cats.Semigroup
import my.cats.wrapper.Wrap

object IdMonadApp {

  def intro(): Unit = {
    import cats.Monad

    /**
     * Without these, the below sumSquare will cause a compilation error:
     *   [error] value flatMap is not a member of type parameter M[Int]
     *   [error]   a.flatMap(x => b.map(y => x*x + y*y))
     *   [error] value map is not a member of type parameter M[Int]
     *   [error]   a.flatMap(x => b.map(y => x*x + y*y))
     * as `M` is not a Monad!!
     */
    import cats.syntax.functor._
    import cats.syntax.flatMap._

    //def sumSquare[M[_](a: M[Int], b: M[Int])(implicit monad : Monad[Int]): M[Int]
    def sumSquare[M[_] : Monad](a: M[Int], b: M[Int]): M[Int] =
      a.flatMap(x => b.map(y => x*x + y*y))
    import cats.instances.option._
    println(sumSquare(Option(3), Option(4)))

    import cats.instances.list._
    println(sumSquare(List(1, 2, 3), List(4, 5)))
  }

  def sumSquar2(): Unit = {
    import cats.Monad

    /**
     * These are still needed for `for` comprehensions too!!
     */
    import cats.syntax.functor._
    import cats.syntax.flatMap._
    def sumSquare[M[_] : Monad](a: M[Int], b: M[Int]): M[Int] =
      for{
        x <- a
        y <- b
      } yield x * y

    import cats.instances.option._
    println(sumSquare(Option(3), Option(4)))

    import cats.instances.list._
    println(sumSquare(List(1, 2, 3), List(4, 5)))

    /***
     * Wazzuuup??? this Id is clearly **NOT** cats Id Monad!!
     * And when you remove the tye, the Id is not available, so it must be using `Id` defined here.
     *
     * As long as (implicit monad : Monad[Int]) is there it works,
     * and for Id[A] no implicit is needed??
     */
    type Id[A] = A
    println(sumSquare(3 : Id[Int], 4 : Id[Int]))
  }

  def catsId(): Unit ={

    import cats.Id
    def pure[A](value: A): Id[A] = value

    def map[A, B](initial: Id[A])(func: A => B): Id[B] = func(initial)

    def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)
  }

  def myId(): Unit = {
    type Id[A] = A
    def pure[A](a: A): Id[A] = a
    def map[A, B](a: A)(f: A => B): Id[B] = f(a)
    def flatMap[A, B](a: A)(f: A => Id[B]): Id[B] = f(a)
  }

//  def sumSquar3(): Unit = {
//    /**
//     * Hmm doesn't work...
//     */
//    import cats.Monad
//    import cats.Semigroup
//
//    /**
//     * These are still needed for `for` comprehensions too!!
//     */
//    import cats.syntax.functor._
//    import cats.syntax.flatMap._
//    import cats.syntax.semigroup._
//
//    def sumSquare[A, M[_] : Monad : Semigroup](a: M[A], b: M[A]): M[A] =
//      for{
//        x <- a
//        y <- b
//      } yield x |+| y
//
//    import cats.instances.option._
//    println(sumSquare(Option(3), Option(4)))
//
//    import cats.instances.list._
//    println(sumSquare(List(1, 2, 3), List(4, 5)))
//  }

  def main(args: Array[String]): Unit = {
    Wrap("intro")(intro())
    Wrap("sumSquar2")(sumSquar2())
  }
}

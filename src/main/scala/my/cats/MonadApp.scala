package my.cats

import my.cats.wrapper.Wrap

object MonadApp {
  def intro(): Unit ={
    def parseInt(str: String): Option[Int] =
      scala.util.Try(str.toInt).toOption
    def divide(a: Int, b: Int): Option[Int] =
      if(b == 0) None else Some(a / b)

    def stringDivideBy(aStr: String, bStr: String): Option[Int] =
      parseInt(aStr).flatMap { aNum =>
        parseInt(bStr).flatMap { bNum =>
          divide(aNum, bNum)
        }
      }

    println(scala.util.Try("1".toInt))
    println(scala.util.Try("0".toInt))
    println(scala.util.Try("a".toInt))
    //Failure(java.lang.NumberFormatException: For input string: "a")

    println(scala.util.Try("a".toInt).toOption)
    //None

    println(stringDivideBy("1", "2"))
    println(stringDivideBy("2", "0"))
    println(stringDivideBy("a", "1"))
  }

  def list(): Unit ={
    val r = for{
      x <- 1 to 10
      y <- 1 to 10
    } yield x * y

    println(r)
    //Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 2, 4, 6, 8, 10, ..., 100)
  }

  def pure(): Unit ={
    import cats.instances.option._
    import cats.syntax.applicative._

    println(1.pure[Option])
    println(1.pure[Option].getClass) //class scala.Some
  }

  def main(args: Array[String]): Unit ={
    Wrap("intro")(intro)
    Wrap("list")(list)
    Wrap("pure")(pure)
  }
}

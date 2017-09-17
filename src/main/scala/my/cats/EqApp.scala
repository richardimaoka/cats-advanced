package my.cats

import my.cats.wrapper.Wrap

object EqApp {

  def lecture(): Unit ={
    import cats.syntax.eq._
    // Both int and option instances are needed
    import cats.instances.int._
    import cats.instances.option._

    // Some(1) === None
    // error: value === is not a member of Some[Int]

    // Some(1) === Some(1)
    // error: value === is not a member of Some[Int]

    Option(1) === Option.empty[Int]
    /**
     * Not in test directory, because ScalaTest mixes in Scalactic,
     * which defines a different `===` method
     */

    import cats.syntax.option._
    println(1.some === Some(1))
    // true
  }

  def exercise(): Unit ={
    val cat1 = Cat("Garfield", 35, "orange and black")
    val cat2 = Cat("Heathcliff", 30, "orange and black")

    import cats.syntax.eq._
    import my.cats.Cat._

    println(cat1 === cat1)
    println(cat1 === cat2)
    println(cat2 === cat2)

    println(cat1 =!= cat1)
    println(cat1 =!= cat2)
    println(cat2 =!= cat2)

    import cats.instances.option._
    val optionCat1 = Option(cat1)
    // optionCat1: Option[Cat] = Some(Cat(Garfield,35,orange and black))
    val optionCat2 = Option.empty[Cat]

    // optionCat2: Option[Cat] = None
    println(optionCat1 === optionCat2)
    // res16: Boolean = false
      println(optionCat1 =!= optionCat2)
    // res17: Boolean = true
  }

  def main(args: Array[String]): Unit = {
    Wrap("lecture")(lecture)
    Wrap("exercise")(exercise)
  }
}

package my.cats

import org.scalatest._

class ShowSpec extends FlatSpec with Matchers {

  def logit(value: => Any): Unit = info(value.toString)

  "cats show" should "return a string" in {
    import cats.Show
    import cats.instances.int._
    // This second import is important, otherwise you get the following erorr
    // "could not find implicit value for parameter instance: cats.Show[Int]"
    val showInt = Show.apply[Int]

    // invoke the show method of the Show type class instance of [Int]
    val actual = showInt.show(10)
    val expected = "10"
    actual should equal (expected)
  }

  "cats show" should "work with a syntax interface" in {
    import cats.syntax.show._
    import cats.instances.int._

    val actual = 123.show
    val expected = "123"
    actual should equal (expected)
  }

  "cats show" should "allow a custom instance for Cat" in {
    import cats.syntax.show._
    import my.cats.Cat._

    val actual = Cat("tama", 10, "white").show
    val expected = "Show: Cat(name = tama, age = 10, color = white)"
    actual should equal (expected)
  }

}

package my.cats

import org.scalatest._

class PrintableSpec extends FlatSpec with Matchers {

  def logit(value: => Any): Unit = info(value.toString)

  "Printable object" should "work for int" in {
    import Printable._
    val expected = "10"
    val actual = Printable.format(10)
    actual should equal (expected)
  }

  "Printable interface object" should "work for int" in {
    import my.cats.Printable._

    val actual = Printable.format(1)
    val expected = "1"
    logit(actual)
    logit(expected)
    actual should equal(expected)
  }

  "Printable interface object" should "print out int" in {
    import my.cats.Printable._
    val actual = Printable.print(1)
  }

  "Printable interface object" should "work for double" in {
    import my.cats.Printable._

    val actual = Printable.format(1.0)
    val expected = "1.0"
    logit(actual)
    logit(expected)
    actual should equal(expected)
  }

  "Printable interface object" should "print out double" in {
    import my.cats.Printable._
    val actual = Printable.print(1.0)
  }

  "Printable interface object" should "work for string" in {
    import my.cats.Printable._

    val actual = Printable.format("spapapa")
    val expected = "spapapa"
    logit(actual)
    logit(expected)
    actual should equal(expected)
  }

  "Printable interface object" should "print out string" in {
    import my.cats.Printable._
    val actual = Printable.print("serafdsafsd")
  }

  "Printable interface object" should "work for person" in {
    /**
     * Person defines the implicit type class instance for Person
     */
    import my.cats.Person._

    val actual = Printable.format(Person("Dave", "dave@example.com"))
    val expected =  "name = Dave, email = dave@example.com"
    logit(actual)
    logit(expected)
    actual should equal(expected)
  }

  "Interface syntax" should "work for int" in {
    import my.cats.PrintableSyntax._

    val actual = 1.format
    val expected = "1"
    logit(actual)
    logit(expected)
    actual should equal(expected)
  }

  "Interface syntax" should "print out int" in {
    import my.cats.PrintableSyntax._
    val actual = 1.print
  }

  "Interface syntax" should "work for double" in {
    import my.cats.PrintableSyntax._

    val actual = 1.0.format
    val expected = "1.0"
    logit(actual)
    logit(expected)
    actual should equal(expected)
  }

  "Interface syntax" should "print out double" in {
    import my.cats.PrintableSyntax._
    val actual = 1.0.print
  }

  "Interface syntax" should "work for Person" in {
    /**
     * import Syntax interface, and
     * import Person's Printable type class instance
     */
    import my.cats.PrintableSyntax._
    import my.cats.Person._

    val actual = Person("Dave", "dave@example.com").format
    val expected =  "name = Dave, email = dave@example.com"
    logit(actual)
    logit(expected)
    actual should equal(expected)
  }

  "Interface syntax" should "print out Person" in {
    import my.cats.PrintableSyntax._
    val actual = Person("Dave", "dave@example.com").print
  }

  "Interface syntax" should "work for Cat" in {
    /**
     * import Syntax interface, and
     * import Person's Printable type class instance
     */
    import my.cats.PrintableSyntax._
    import my.cats.Cat._

    val actual = Cat("Dave", 205, "red").format
    val expected = "Cat(name = Dave, age = 205, color = red)"
    logit(actual)
    logit(expected)
    actual should equal(expected)
  }

  "Interface syntax" should "print out Cat" in {
    import my.cats.PrintableSyntax._
    Cat("Dave", 205, "red").print
  }

}

package my.cats

import cats.kernel.Eq

final case class Cat(
  name: String,
  age: Int,
  color: String
)

object Cat {
  implicit val catPrintable = new Printable[Cat] {
    def format(cat: Cat): String = s"Cat(name = ${cat.name}, age = ${cat.age}, color = ${cat.color})"
  }

  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  implicit val catShow = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }

  import cats.syntax.eq._
  implicit val catEqual = Eq.instance[Cat] { (cat1, cat2) =>
    cat1.name === cat2.name &&
    cat1.age === cat2.age &&
    cat1.color === cat2.color
  }
}

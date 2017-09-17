package my.cats

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
}

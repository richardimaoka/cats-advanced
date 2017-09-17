package my.cats

import cats.Show

final case class Cat(
  name: String,
  age: Int,
  color: String
)

object Cat {
  implicit val catPrintable = new Printable[Cat] {
    def format(cat: Cat): String = s"Cat(name = ${cat.name}, age = ${cat.age}, color = ${cat.color})"
  }

  implicit val catShow = new Show[Cat] {
    def show(cat: Cat): String = s"Show: Cat(name = ${cat.name}, age = ${cat.age}, color = ${cat.color})"
  }
}

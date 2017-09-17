package my.cats

trait Printable[A] {
  def format(a: A): String

  def print(a: A): Unit = println(format(a))
}

object Printable {
  def format[A](a: A)(implicit printable: Printable[A]): String = printable.format(a)

  def print[A](a: A)(implicit printable: Printable[A]): Unit = printable.print(a)

  implicit val intPrintable = new Printable[Int]{
    def format(int: Int): String = int.toString
  }

  implicit val doublePrintable = new Printable[Double]{
    def format(double: Double): String = double.toString
  }

  implicit val stringPrintable = new Printable[String]{
    def format(str: String): String = str
  }
}

object PrintableSyntax {
  implicit class PrintableSyntaxOps[A](value: A) {
    def format(implicit p: Printable[A]): String =
      p.format(value)

    def print(implicit p: Printable[A]): Unit =
      p.print(value)
  }
}

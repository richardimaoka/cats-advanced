package my.cats

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class MonadSpec extends FlatSpec with Matchers with PropertyChecks {

  def logit(value: => Any): Unit = info(value.toString)

  /***********************************
   * Specific case
   ***********************************/
  "Monad" should "satisfy left identity" in {
    import cats.instances.option._
    import cats.syntax.applicative._

    val a = 1
    val f: Int => Option[String] = i => Some((i * 2).toString + " yeah")

    logit(f(a))
    logit(a.pure[Option].flatMap(f))

    a.pure[Option].flatMap(f) should be(f(a))
  }

  "Monad" should "satisfy right identity" in {
    import cats.instances.option._
    import cats.syntax.applicative._

    val m = Option(30)
    logit(m.flatMap(_.pure))

    m.flatMap(_.pure) should be (m)
  }

  "Monad" should "satisfy associativity" in {
    import cats.instances.option._
    import cats.syntax.applicative._

    val m = Option(30)

    val f: Int => Option[String] = i => Some((i * 2).toString + " yeah")
    val g: String => Option[String] = str => Some( str + " : " + str )

    val left  = m.flatMap(f).flatMap(g)
    val right = m.flatMap(x => f(x).flatMap(g))

    logit(left)
    logit(right)

    left should be (right)
  }


  def checkLeftIdentity[A, B, M[A]](f: A => M[A]): Unit = forAll {
    import cats.instances.option._
    import cats.syntax.applicative._

    a.pure[M].flatMap(f) should be(f(a))
  }

  /***********************************
   * Using Generator for Option
   ***********************************/

  "Monad[Option[Int]]" should "satisfy left identity" in {
    forAll { (a: Int) =>
      import cats.instances.option._
      import cats.syntax.applicative._

      //logit(s"testing for $i")
      val f: Int => Option[String] = i => Some((i * 2).toString + " yeah")

      a.pure[Option].flatMap(f) should be(f(a))
    }
  }

  /***********************************
   * Using Generator for List
   ***********************************/

  "Monad[List[Int]]" should "satisfy left identity" in {
    forAll { (a: Int) =>
      import cats.instances.list._
      import cats.syntax.applicative._

      //logit(s"testing for $i")
      val f: Int => List[String] = i => List((i * 2).toString + " yeah")

      logit(f(a))
      logit(a.pure[List].flatMap(f))

      a.pure[List].flatMap(f) should be(f(a))
    }
  }
}

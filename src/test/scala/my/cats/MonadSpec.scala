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


//  def checkLeftIdentity[A, B, M[_]: Monad[M]](f: A => M[A]): Unit = forAll {
//    import cats.instances.option._
//    import cats.syntax.applicative._
//
//    val aMonad = a.pure[M[A]]
//      aMonad..flatMap(f) should be(f(a))
//  }

  /***********************************
   * Using Generator for Option
   ***********************************/

  "Monad[Option[Int]]" should "satisfy left identity" in {
    import cats.instances.option._
    import cats.syntax.applicative._

    val f: Int => Option[String] = i => Some((i * 2).toString + " yeah")

    forAll { (a: Int) =>
      //logit(s"testing for $i")
      a.pure[Option].flatMap(f) should be(f(a))
    }
  }

  /***********************************
   * Using Generator for List
   ***********************************/

  "Monad[List[Int]]" should "satisfy left identity" in {
    import cats.instances.list._
    import cats.syntax.applicative._

    val f: Int => List[String] = i => List((i * 2).toString + " yeah")

    forAll { (a: Int) =>
      //logit(s"${f(a)} should be ${a.pure[List].flatMap(f)}")
      a.pure[List].flatMap(f) should be(f(a))
    }
  }

  /***********************************
   * Using function and generator
   ***********************************/
  import cats.Monad

  def verifyLeftIdentityLaw[A, B, F[_]](a: A, log: Boolean = false)(f: A => F[B])(implicit applicative: Monad[F]): Unit = {
    import cats.syntax.applicative._
    import cats.syntax.flatMap._

    if(log) {
      logit(s"testing for $a")
      logit(s"f(a) = ${f(a)} should be a.pure[F].flatMap(f) = ${a.pure[F].flatMap(f)}")
    }
    a.pure[F].flatMap(f) should be(f(a))
  }

  def verifyRightIdentityLaw[A, F[_]](m: F[A], log: Boolean = false)(implicit applicative: Monad[F]): Unit = {
    import cats.syntax.applicative._
    import cats.syntax.flatMap._

    if(log) {
      logit(s"m.flatMap(_.pure) = ${m.flatMap(_.pure)} should be m = ${m}")
    }
    m.flatMap(_.pure) should be(m)
  }

  def verifyAssociativityLaw[A, B, C, F[_]](m: F[A], log: Boolean = false)(f: A => F[B], g: B => F[C])(implicit applicative: Monad[F]): Unit = {
    import cats.syntax.flatMap._

    if(log) {
      logit(s"testing for $m")
      logit(s"m.flatMap(f).flatMap(g)) = ${m.flatMap(f).flatMap(g)} should be m.flatMap(x => f(x).flatMap(g)) = ${m.flatMap(x => f(x).flatMap(g))}")
    }
    m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
  }

  def runNamedTest(name: String, test: => Unit): Unit = {
    logit("running test = " + name)
    test
  }

  "Monad[Option[Int]]" should "satisfy left identity 2" in {
    import cats.instances.option._

    case class MyException(val message: String) extends Exception {
      override def equals(that: Any): Boolean = {
        this.getClass == that.getClass &&
        this.message == that.asInstanceOf[MyException].message
      }
    }

    val f0: Int => Option[String] = i => Some((i * 2).toString + " yeah")
    val f1: Int => Option[String] = _ => None
    val f2: Int => Option[Int] = i => Some(i*i)

    /**
     * The Java default Exception's equals method returns true,
     * only when they are exactly the same instance (memory reference)
     *
     * So, using my own Exception type which only checks the equality of the error message
     *  => test passes
     */
    val f3: Int => Option[Throwable] = i => Some(new MyException(s"Exception: $i"))
    val f4: Int => Option[MyException] = i => Some(new MyException(s"Exception: $i"))

    val f5: Int => Option[Either[Throwable, String]] = i =>
      if(i % 2 == 0)
        Some(Right(s"even number ${i} allowed"))
      else
        Some(Left(new MyException(s"odd number ${i} is not allowed ")))
    val f6: Int => Option[List[Int]] = i => Some((0 to i).toList)
    val f7: Int => Option[String] =
      i =>
        Map(
          0 -> "here",
          2 -> "here",
          4 -> "here",
          5 -> "here",
          6 -> "here",
          11 -> "here",
          13 -> "here",
          -12 -> "here",
          10 -> "here"
        ).get(i)

    forAll { (a: Int) => {
      runNamedTest("test0", verifyLeftIdentityLaw(a)(f0))
      runNamedTest("test1", verifyLeftIdentityLaw(a)(f1))
      runNamedTest("test2", verifyLeftIdentityLaw(a)(f2))
      runNamedTest("test3", verifyLeftIdentityLaw(a)(f3))
      runNamedTest("test4", verifyLeftIdentityLaw(a)(f4))
      runNamedTest("test5", verifyLeftIdentityLaw(a)(f5))
      runNamedTest("test6", verifyLeftIdentityLaw(a)(f6))
      runNamedTest("test7", verifyLeftIdentityLaw(a)(f7))
    }}
  }
}

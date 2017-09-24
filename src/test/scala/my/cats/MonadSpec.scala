package my.cats

import org.scalacheck.Gen
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
    val m = Option(30)

    val f: Int => Option[String] = i => Some((i * 2).toString + " yeah")
    val g: String => Option[String] = str => Some( str + " : " + str )

    val left  = m.flatMap(f).flatMap(g)
    val right = m.flatMap(x => f(x).flatMap(g))

    logit(left)
    logit(right)

    left should be (right)
  }

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
    a.pure[F].flatMap(f) should be (f(a))
  }

  def verifyRightIdentityLaw[A, F[_]](m: F[A], log: Boolean = false)(implicit applicative: Monad[F]): Unit = {
    import cats.syntax.applicative._
    import cats.syntax.flatMap._

    if(log) {
      logit(s"m.flatMap(_.pure) = ${m.flatMap(_.pure)} should be m = ${m}")
    }
    m.flatMap(_.pure) should be (m)
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
    logit("fnished " + name)
  }

  "Monad[Option[Int]]" should "satisfy left identity with verifiers" in {
    import cats.instances.option._

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
    case class MyException(val message: String) extends Exception {
      override def equals(that: Any): Boolean = {
        this.getClass == that.getClass &&
          this.message == that.asInstanceOf[MyException].message
      }
    }
    val f3: Int => Option[Throwable] = i => Some(new MyException(s"Exception: $i"))
    val f4: Int => Option[MyException] = i => Some(new MyException(s"Exception: $i"))
    /**
     * In contrast, this one with the Java default Exception is not run for test as it will fail.
     * You can mix in CatsSuite which overrides catsCanEqual via StrictCatsEquality trait,
     * and that allows you call === calling underlying Cats Eq's equality
     */
    //val f: Int => Option[Throwable] = i => Some(new Exception(s"Exception: $i"))
    val f5: Int => Option[Either[Throwable, String]] = i =>
      if(i % 2 == 0)
        Some(Right(s"even number ${i} allowed"))
      else
        Some(Left(new MyException(s"odd number ${i} is not allowed ")))
    val f6: Int => Option[List[Int]] = i => Some((0 to i).toList)
    val f7: Int => Option[String] = i => Map( 0 -> "here", 2 -> "here", 4 -> "here", 5 -> "here" ).get(i)

    runNamedTest("test0", forAll{(a: Int) => verifyLeftIdentityLaw(a)(f0)})
    runNamedTest("test1", forAll{(a: Int) => verifyLeftIdentityLaw(a)(f1)})
    runNamedTest("test2", forAll{(a: Int) => verifyLeftIdentityLaw(a)(f2)})
    runNamedTest("test3", forAll{(a: Int) => verifyLeftIdentityLaw(a)(f3)})
    runNamedTest("test4", forAll{(a: Int) => verifyLeftIdentityLaw(a)(f4)})
    runNamedTest("test5", forAll{(a: Int) => verifyLeftIdentityLaw(a)(f5)})
    val upto100 = for(n <- Gen.choose(0, 100)) yield n
    runNamedTest("test6", forAll(upto100){(a: Int) => verifyLeftIdentityLaw(a)(f6)})
    runNamedTest("test7", forAll{(a: Int) => verifyLeftIdentityLaw(a)(f7)})
  }

  "Monad[Option[Int]]" should "satisfy right identity with verifiers" in {
    import cats.instances.option._

    runNamedTest("test0", forAll{(a: Int) => verifyRightIdentityLaw(Option(a))})
    runNamedTest("test1", forAll{(a: Int) => verifyRightIdentityLaw(None)})
    runNamedTest("test2", forAll{(a: Option[Int]) => verifyRightIdentityLaw(a)})
    runNamedTest("test3", forAll{(a: Option[String]) => verifyRightIdentityLaw(a)})
    runNamedTest("test4", forAll{(a: Option[Long]) => verifyRightIdentityLaw(a)})
    runNamedTest("test5", forAll{(a: Option[Double]) => verifyRightIdentityLaw(a)})
    val upto100 = for(n <- Gen.choose(0, 100)) yield n
    runNamedTest("test6", forAll(upto100){(a: Int) => verifyRightIdentityLaw(Option((0 to a).toList))})
    runNamedTest("test7", forAll{(a: Option[Option[Int]]) => verifyRightIdentityLaw(a)})

    case class MyException(val message: String) extends Exception {
      override def equals(that: Any): Boolean = {
        this.getClass == that.getClass &&
          this.message == that.asInstanceOf[MyException].message
      }
    }
    runNamedTest("test8", forAll{(a: String) => verifyRightIdentityLaw(Option(new MyException(a)))})
  }

  "Monad[Option[Int]]" should "satisfy associativity with verifiers" in {
    import cats.instances.option._

    val f1: Int => Option[Double] = i => Some(i)
    val g1: Double => Option[String] = i => Some(i.toString)

    val f2: Int => Option[Double] = i => None
    val g2: Double => Option[String] = i => None

    val f3: Int => Option[Option[Double]] = o => Some(Some(o))
    val g3: Option[Double] => Option[Option[String]] = o => o match {
      case Some(d) => Some(Some(d.toString))
      case None => None
    }

    val f4: Int => Option[Double] = i => Some(i*i)
    val g4: Double => Option[String] = i => Some(i.toString)

    case class MyException(val message: String) extends Exception {
      override def equals(that: Any): Boolean = {
        this.getClass == that.getClass &&
          this.message == that.asInstanceOf[MyException].message
      }
    }
    val f5: Int => Option[Throwable] = i => Some(new MyException(s"Exception: $i"))
    val g5: Throwable => Option[String] = i => Some(i.toString)

    val f6: Int => Option[MyException] = i => Some(new MyException(s"Exception: $i"))
    val g6: MyException => Option[String] = i => Some(i.toString)

    /**
     * In contrast, this one with the Java default Exception is not run for test as it will fail.
     * You can mix in CatsSuite which overrides catsCanEqual via StrictCatsEquality trait,
     * and that allows you call === calling underlying Cats Eq's equality
     */
    //val f: Int => Option[Throwable] = i => Some(new MyException(s"Exception: $i"))
    //val g: Throwable => Option[String] = i => Some(i.toString)

    val f7: Int => Option[Either[Throwable, String]] = i =>
      if(i % 2 == 0)
        Some(Right(s"even number ${i} allowed"))
      else
        Some(Left(new MyException(s"odd number ${i} is not allowed ")))
    val g7: Either[Throwable, String] => Option[String] =
      either => either.fold(
        throwable => Some("you've got a throwable!" + throwable.getMessage),
        string => Some("congrats! here's your string = " + string)
      )

    runNamedTest("test1", forAll{(a: Int) => verifyAssociativityLaw(Option(a))(f1, g1)})
    runNamedTest("test2", forAll{(a: Int) => verifyAssociativityLaw(Option(a))(f2, g2)})
    runNamedTest("test3", forAll{(a: Int) => verifyAssociativityLaw(Option(a))(f3, g3)})
    runNamedTest("test4", forAll{(a: Int) => verifyAssociativityLaw(Option(a))(f4, g4)})
    runNamedTest("test5", forAll{(a: Int) => verifyAssociativityLaw(Option(a))(f5, g5)})
    runNamedTest("test6", forAll{(a: Int) => verifyAssociativityLaw(Option(a))(f6, g6)})
    runNamedTest("test7", forAll{(a: Int) => verifyAssociativityLaw(Option(a))(f7, g7)})
  }

  "Monad[List[Int]]" should "satisfy left identity with verifiers" in {
    import cats.instances.list._

    val f0: Int => List[Int] = i => (1 to i).toList
    val f1: Int => List[Int] = _ => List[Int]()
    val f2: Int => List[String] = i => List(i.toString)
    /**
     * The Java default Exception's equals method returns true,
     * only when they are exactly the same instance (memory reference)
     *
     * So, using my own Exception type which only checks the equality of the error message
     *  => test passes
     */
    case class MyException(val message: String) extends Exception {
      override def equals(that: Any): Boolean = {
        this.getClass == that.getClass &&
          this.message == that.asInstanceOf[MyException].message
      }
    }
    val f3: Int => List[Throwable] = i => List(new MyException(s"Exception: $i"))
    val f4: Int => List[MyException] = i => List(new MyException(s"Exception: $i"))
    /**
     * In contrast, this one with the Java default Exception is not run for test as it will fail.
     * You can mix in CatsSuite which overrides catsCanEqual via StrictCatsEquality trait,
     * and that allows you call === calling underlying Cats Eq's equality
     */
    //val f: Int => List[Throwable] = i => List(new Exception(s"Exception: $i"))
    val f5: Int => List[Either[Throwable, String]] = i =>
      if(i % 2 == 0)
        List(Right(s"even number ${i} allowed"))
      else
        List(Left(new MyException(s"odd number ${i} is not allowed ")))
    val f6: Int => List[List[Int]] = i => List((0 to i).toList)

    val upto100 = for(n <- Gen.choose(0, 100)) yield n
    runNamedTest("test0", forAll(upto100){(a: Int) => verifyLeftIdentityLaw(a)(f0)})
    runNamedTest("test1", forAll{(a: Int) => verifyLeftIdentityLaw(a)(f1)})
    runNamedTest("test2", forAll{(a: Int) => verifyLeftIdentityLaw(a)(f2)})
    runNamedTest("test3", forAll{(a: Int) => verifyLeftIdentityLaw(a)(f3)})
    runNamedTest("test4", forAll{(a: Int) => verifyLeftIdentityLaw(a)(f4)})
    runNamedTest("test5", forAll{(a: Int) => verifyLeftIdentityLaw(a)(f5)})
    runNamedTest("test6", forAll(upto100){(a: Int) => verifyLeftIdentityLaw(a)(f6)})
  }

  "Monad[List[Int]]" should "satisfy right identity with verifiers" in {
    import cats.instances.list._

    runNamedTest("test0", forAll{(a: Int) => verifyRightIdentityLaw(List(a))})
    runNamedTest("test1", forAll{(a: Int) => verifyRightIdentityLaw(Nil)})
    runNamedTest("test2", forAll{(a: List[Int]) => verifyRightIdentityLaw(a)})
    runNamedTest("test3", forAll{(a: List[String]) => verifyRightIdentityLaw(a)})
    runNamedTest("test4", forAll{(a: List[Long]) => verifyRightIdentityLaw(a)})
    runNamedTest("test5", forAll{(a: List[Double]) => verifyRightIdentityLaw(a)})
    val upto100 = for(n <- Gen.choose(0, 100)) yield n
    runNamedTest("test6", forAll(upto100){(a: Int) => verifyRightIdentityLaw((0 to a).toList)})
    runNamedTest("test7", forAll{(a: List[Option[Int]]) => verifyRightIdentityLaw(a)})

    case class MyException(val message: String) extends Exception {
      override def equals(that: Any): Boolean = {
        this.getClass == that.getClass &&
          this.message == that.asInstanceOf[MyException].message
      }
    }
    runNamedTest("test8", forAll{(a: String) => verifyRightIdentityLaw(List(new MyException(a)))})
  }
}

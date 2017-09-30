package my.cats

import my.cats.wrapper.Wrap

object EitherApp {
  def intro(): Unit ={
    // Valid in Scala 2.11 and Scala 2.12

    val either1: Either[String, Int] = Right(123)
    // either1: Either[String,Int] = Right(123)
    val either2: Either[String, Int] = Right(321)
    // either2: Either[String,Int] = Right(321)

    println(either1)
    //Right(123)
    println(either1.right)
    //RightProjection(Right(123))
    println(either1.left)
    //LeftProjection(Right(123))

    println(either1.flatMap(x => Right(x * 2)))
    // Right(246)
    println(either1.right.flatMap(x => Right(x * 2)))
    // res2: scala.util.Either[String,Int] = Right(246)

    println(either2.left.flatMap(x => Left(x + "!!!")))
    // Right(321)

    println(either2.left.flatMap(x => Left(x + "!!!")))
    // res3: scala.util.Either[String,Int] = Right(321)

    println(either2.left.flatMap(x => Left(x + "!!!")).right.flatMap(x => Right(x*x)))
    // Right(103041)

    println(Right[String, Int](100)
      .left.flatMap(x => Left(x + "!!!"))
      .right.flatMap(x => Right(x*x))
    )
    //Right(10000)
    println(Right[String, Int](100).fold(
      x => x + "!!!",
      x => x*x
    ))
    //10000

    println(Left[String, Int]("bah")
      .left.flatMap(x => Left(x + "!!!"))
      .right.flatMap(x => Right(x*x))
    )
    //Right("bah!!!")
    println(Left[String, Int]("bah").fold(
      x => x + "!!!",
      x => x*x
    ))
    //"bah!!!"
  }

  def design(): Unit ={
    case class CompanyId(id: String)
    case class User(name: String, companyID: CompanyId)
    case class UserId(id: String)

    def getUserFromDatabase(uid: UserId): User = {
      if(uid.id == "exists")
        User("John doh", CompanyId("GE"))
      else
        throw new Exception("User Does not exist")
    }

    try{
      getUserFromDatabase(UserId("nonexist"))
    } catch {
      case e: Exception => println(e)
      //java.lang.Exception: User Does not exist
    }

    def getUserFromDatabaseEither(uid: UserId): Either[Exception, User] = {
      if(uid.id == "a")
        Right(User("John doh", CompanyId("GE")))
      else if(uid.id == "b")
        Right(User("Gary Cohn", CompanyId("GS")))
      else
        Left(new Exception("User Does not exist"))
    }
    def getCompanyDetailsFromDatabaseEither(companyId: CompanyId): Either[Exception, Map[String, Any]] = {
      if(companyId.id == "GE")
        Right(Map("a" -> 1, "b" -> 2))
      else {
        Left(new Exception("Company not exist"))
      }
    }

    println(getUserFromDatabaseEither(UserId("nonexist")))
    // Left(java.lang.Exception: User Does not exist)

    def getCompanyDetailsForUser(uid: UserId) =
      for{
        user <- getUserFromDatabaseEither(uid)
        details <- getCompanyDetailsFromDatabaseEither(user.companyID)
      } yield details

    println(getCompanyDetailsForUser(UserId("a")))
    // Right(Map(a -> 1, b -> 2))

    println(getCompanyDetailsForUser(UserId("b")))
    // Left(java.lang.Exception: Company not exist)

    println(getCompanyDetailsForUser(UserId("non existent")))
    // Left(java.lang.Exception: User Does not exist)
  }

  def main(array: Array[String]): Unit ={
    Wrap("intro")(intro)
    Wrap("design")(design)
  }
}

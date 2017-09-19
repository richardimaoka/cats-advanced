package my.cats

import my.cats.wrapper.Wrap

object FunctorApp {

  def various(): Unit ={
    println(Option(1) map (Option(_)))
    println(Map(1 -> "a").map(a => (a._1*2, a._2+" yeah")))
  }

  def future(): Unit ={
    import scala.concurrent.{Future, Await}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val future1 = Future("Hello world!")
    // future1: scala.concurrent.Future[String] = Future(<not completed>)
    val future2 = future1.map(_.length)
    // future2: scala.concurrent.Future[Int] = Future(<not completed >)

    println(Await.result(future1, 1.second))
    // res6: String = Hello world!

    println(Await.result(future2, 1.second))
    // res7: Int = 12

    val sideEffectFuture = Future({println("side effect"); 10})
    println(Await.result(sideEffectFuture.map(_ * 10), 1.second))
    println(Await.result(sideEffectFuture.map(_ * 10).map(_ * 10), 1.second))
    //println(Await.result(sideEffectFuture.map(a => throw new Exception("bah")).map(_ => 10), 1.second))
  }

  def main(args: Array[String]): Unit ={
    Wrap("various functors")(various)
    Wrap("future")(future)
  }
}

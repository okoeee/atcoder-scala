package example

import scala.concurrent.{Future, blocking}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success}

object FutureSample {

  def exe = {

    val f: Future[Int] = Future {
      blocking {
        Thread.sleep(1000)
      }
      24
    }

    f
  }

  // 実行のとき
//  try {
//    val futureResult = FutureSample.exe
//    val result = Await.result(futureResult, Duration.Inf)
//    println(result)
//  } catch {
//    case ex: Throwable => println(ex)
//  }

  def get(url: String): Future[BufferedSource] = Future(Source.fromURL(url))

  //実行のとき
//  try {
//    val futureResult = FutureSample.get("https://scalamatsuri.org/en/")
//    val result = Await.result(futureResult, Duration.Inf)
//    println(result.mkString)
//  } catch {
//    case exception: Throwable => println(exception)
//  }
}

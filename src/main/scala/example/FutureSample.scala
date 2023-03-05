package example

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object FutureSample {

  def exe = {

    val responseFuture = Future(HttpTextClient.get("https://change-x.jp/"))

    responseFuture.onComplete {
      case Success(body) =>
        println(body.mkString)
        body.close
      case Failure(exception) => println("エラーが発生" + exception.toString)
    }

  }

}

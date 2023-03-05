package example

import scala.io.Source

object HttpTextClient {

  def get(url: String) = {
    Source.fromURL(url)
  }

}

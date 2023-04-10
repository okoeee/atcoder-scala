package example

object Monoxer {

  def arrayChallenge(strArr: Array[String]) = {
    // inputされる値
    // Array("B:-1", "A:1", "B:3", "A:5", "C:4")
    val r = strArr
      .map { str =>
        val Array(k, v) = str.split(":")
        (k, v.toInt) // タプルで返す
      }
      .groupBy(_._1) // keyでグループ化 Map("A" -> Array(("A", 1), ("A", 5)), "B" -> Array(("B", 3)))

    r.map { v =>
      val result = v._2.map(_._2).sum
      println(s"${v._1}:${result}")
    }
  }

}

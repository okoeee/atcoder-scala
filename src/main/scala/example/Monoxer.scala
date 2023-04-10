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

  def arrayChallengeOther(strArr: Array[String]) = {
    val r = strArr
      .map { str =>
        val Array(k, v) = str.split(":")
        (k, v.toInt)
      }
      .groupBy(_._1)

    val a = r.mapValues { arr =>
      arr.map(v => v._2).sum
    }

    a.filter(_._2 != 0).toSeq.sorted.map { case (k, v) => println(s"$k:$v") }

  }

  //  関数ArrayChallenge(arr) は 、arrに格納された数字の配列を受け取り 、x日に株を買い 、y日に株を売り 、y ＞ xとなった場合の最大利益を返すようにします
  //  。例えば 、arrが[44, 30, 24, 32, 35, 30, 40, 38, 15] の場合 、プログラムは16を返すはずである
  //  もしこの株価で利益が出なかった場合プログラムは - 1 を返さなければなりません例arrが[10, 9, 8, 2] の場合プログラムは - 1 を返さなければならない
  def calcStock(arr: Array[Int]) = {
    val result = (for {
      i <- 0 until arr.length - 1
      j <- i until arr.length
    } yield arr(j) - arr(i)).max
    println(result)
  }

}

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

  // 関数MathChallenge(num)が、与えられた数字がフィボナッチ数列の一部である場合、文字列yesを返すようにします。
  // この数列は次のように定義される： Fn＝Fn-1＋Fn-2、つまり、Fnを求めるには、前の2つの数字を足し算する。最初の2つの数字は0と1であり、次に1、2、3、5と続く。
  // numがフィボナッチ数列でない場合は、no.という文字列を返す。
  def isFibonacciSeq(num: Int) = {

    def calc(preNum: Int, nextNum: Int): String = {
      if (preNum == num) "yes"
      else if (preNum > num) "no"
      else {
        calc(nextNum, preNum + nextNum)
      }
    }

    val ans = calc(1, 1)
    println(ans)

  }

}

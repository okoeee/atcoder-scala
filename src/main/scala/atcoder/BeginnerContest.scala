package atcoder

object BeginnerContest {
  def ABC285_A: Unit = {
    println("Input value")
    val list = io.StdIn.readLine().split(" ").map(_.toInt)
    val (a, b) = (list(0), list(1))
    if (a / 2 == b || a * 2 == b || a * 2 + 1 == b) {
      println("Yes")
    } else {
      println("No")
    }
  }

  def ABC285_B: Unit = {
    println("input value")
    val numOfString = io.StdIn.readInt
    val inputList = io.StdIn.readLine.split("")
    val a = for (i <- 1 to numOfString - 1) yield {
      var b = 0
      for (k <- 1 to numOfString - i) yield {
        if (inputList(k - 1) == inputList(k - 1 + i)) {
          b = k - 1
          println(b)
        } else b = k
      }
      b
    }
    // println(a)
  }

  def ABC285_C: Unit = {
    println("input value")
    val input = io.StdIn.readLine.split("")
    val numOfDigits = input.length
    val alphabet = Seq("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
    val alphabetMap = alphabet.zipWithIndex.map { case (value, index) =>
      value -> (index + 1)
    }.toMap
    val result = for (i <- 0 to numOfDigits - 1) yield {
      val key = input(numOfDigits - i - 1)
      BigInt((Math.pow(26, i) * alphabetMap(key)).toInt)
    }

    println(result)
    println(result.sum)
  }

  def ABC285_D: Unit = {
    println("input value")
    val num = io.StdIn.readInt
    def returnSAndT(num: Int, s: Seq[String], t: Seq[String]): (Seq[String], Seq[String]) = {
      if (num != 0) {
        val list = io.StdIn.readLine.split(" ")
        val newS = s :+ list(0)
        val newT = t :+ list(1)
        returnSAndT(num - 1, newS, newT)
      } else {
        (s, t)
      }
    }
    val (s, t) = returnSAndT(num, Seq(), Seq())
    def returnCompleted(num: Int, s: Seq[String], t: Seq[String], r: Boolean): Boolean = {
      if (num != 0 && r == true) {
        val i = t.zipWithIndex.map { case (v, index) =>
          val r = s.indexOf(v)
          if (r == -1) index
          else -1
        }
        // iから-1以外の最初の数字を探す
        val matchedListNum = i.find(_ > -1)
        matchedListNum match {
          case Some(x) =>
            // s, tからmatchしたindexの値を削除
            val newS = s diff Seq(s(x))
            val newT = t diff Seq(t(x))
            returnCompleted(num - 1, newS, newT, true)
          case _ =>
            // falseを返す
            returnCompleted(0, s, t, false)
        }
      } else {
        r
      }
    }

    if (returnCompleted(num, s, t, true)) println("Yes")
    else println("No")
  }

}

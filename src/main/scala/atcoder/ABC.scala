package atcoder

object ABC {

  def bitWiseExclusiveOr = {

    val in = new java.util.Scanner(System.in)
    val A, B = in.nextInt

    println("input value")
    io.StdIn.readLine.split(" ").map(_.toInt).toList match {
      case head :: tail =>
        println(head ^ tail(0))
      case _ =>
    }
  }

  def boobyPrize = {
    println("input value")
    io.StdIn.readInt
    val result = io.StdIn.readLine.split(" ").zipWithIndex.map { case (v, i) => (v.toInt, i + 1) }.sortWith((o1, o2) => o1._1 > o2._1)
    println(result(1)._2)
  }

  def howMany = {
    println("input value")
    io.StdIn.readLine.split(" ").map(_.toInt).toList match {
      case head :: last =>
        val s = head
        val t = last(0)
        val rList =
          for (
            a <- 0 to s;
            b <- 0 to (s - a);
            c <- 0 to (s - a - b)
          ) yield {
            if (a + b + c <= s && a * b * c <= t) 1
            else 0
          }
        println(rList.sum)
      case _ =>
    }
  }

  def log2 = {
    println("input value")
    val n = io.StdIn.readLong
    var k = 0
    var t = 1L
    while (t * 2 <= n) {
      k += 1
      t *= 2
    }
    println(k)
  }

  def oneMoreAabAbaBaa = {
    println("input value")
    io.StdIn.readLine.split(" ").toList match {
      case List(s, k) =>
        s.split("").toList.map { v => }
    }
  }

  def signedDifficulty = {
    println("input value")
    val result = io.StdIn.readLine.split("\\.").map(_.toInt).toList match {
      case List(x, y) =>
        if (y >= 0 && y <= 2) s"$x-"
        else if (y >= 3 && y <= 6) s"$x"
        else if (y >= 7 && y <= 9) s"$x+"
      case x: Any => println(x)
    }
    println(result)
  }

  def sameName = {
    val n = io.StdIn.readInt

    def cal(n: Int, preList: List[(String, String)]): String = {
      if (n == 0) "No"
      else
        io.StdIn.readLine.split(" ").toList match {
          case List(x, y) =>
            val hoge = preList.exists(p => p._1 == x && p._2 == y)
            if (hoge) "Yes"
            else cal(n - 1, (x, y) +: preList)
        }
    }

    println(cal(n, List(): List[(String, String)]))
  }

  def sameNameOther = {
    val n = io.StdIn.readInt
    val list = List.fill(n)(io.StdIn.readLine)
    println {
      if (list.size == list.toSet.size) "No"
      else "Yes"
    }
  }

  def lexicographicOrder = {
    val list = io.StdIn.readLine.split(" ").toList
    val sortedList = list.sortWith(_ < _)
    println {
      if (list == sortedList) "Yes"
      else "No"
    }
  }

  def atcoderQuiz = {
    val list = List.fill(3)(io.StdIn.readLine)
    val result = List("ABC", "ARC", "AGC", "AHC").filter(p => !list.contains(p)).head
    println(result)
  }

  def atcoderQuizOther = {
    val list = List.fill(3)(io.StdIn.readLine)
    val result = List("ABC", "ARC", "AGC", "AHC").diff(list).head
    println(result)
  }

  def inverseOfPermutation = {
    val _ = io.StdIn.readInt
    val q = io.StdIn.readLine.split(" ").zipWithIndex.map { case (p, index) => (p.toInt, index + 1) }
    val sortedQ = q.sortBy(_._1)
    val result = sortedQ.foldLeft("") { (acc, x) => acc + " " + (x._2).toString }.trim
    println(result)
  }

  def inverseOfPermutationOther = {
    val in = new java.util.Scanner(System.in)
    val N = in.nextInt
    val Qs = List.fill(N)(in.nextInt)
    println(Qs.zipWithIndex.sorted.map(_._2 + 1).mkString(" "))
  }

  def weatherForecast = {
    val n = io.StdIn.readInt
    val list = io.StdIn.readLine.split("")
    if (list(n - 1) == "o") println("Yes")
    else println("No")
  }

  def weatherForecastOther = {
    val n = io.StdIn.readInt
    val list = io.StdIn.readLine
    list.charAt(n - 1)
  }

  def qwerty = {
    val A = "abcdefghijklmnopqrstuvwxyz"
    val list = io.StdIn.readLine.split(" ")
    val result = list.map { v => A.charAt(v.toInt - 1) }.mkString
    println(result)
  }

  def shapesOther = {
    val in = new java.util.Scanner(System.in)
    val N = in.nextInt
    val S = List.fill(N)(in.next.toList)
    val T = List.fill(N)(in.next.toList)

    println(s"""
         |S = $S
         |T = $T
         |""".stripMargin)

    def rotate(g: List[List[Char]]): List[List[Char]] = g.transpose.map(_.reverse)
    def trimAndRotate(g: List[List[Char]]): List[List[Char]] = rotate(g.dropWhile(_.forall(_ == '.')))

    val trimedS = (1 to 4).foldLeft(S)((s, _) => trimAndRotate(s))
    val trimedT = (1 to 4).foldLeft(T)((t, _) => trimAndRotate(t))
    val allS = (1 to 3).scanLeft(trimedS)((s, _) => rotate(s))

    println(s"$trimedS")

    println(if (allS.contains(trimedT)) "Yes" else "No")
  }

  def atCoderQuiz2 = {
    val x = io.StdIn.readInt
    if (x >= 0 && x < 40) println(40 - x)
    else if (x >= 40 && x < 70) println(70 - x)
    else if (x >= 70 && x < 90) println(90 - x)
    else println("expert")
  }

  def maritozzo = {
    val s1 = io.StdIn.readLine
    val s2 = io.StdIn.readLine
    val s3 = io.StdIn.readLine
    val list = io.StdIn.readLine.split("").map { v =>
      v.toInt match {
        case 1 => s1
        case 2 => s2
        case 3 => s3
        case _ => // do nothing
      }
    }
    println(list.mkString)
  }

  def neoLexicographicOrdering = {
    val in = new java.util.Scanner(System.in)
    val X = in.next
    val N = in.nextInt
    val Ss = List.fill(N)(in.next)

    val sortedList = Ss.sortBy(_.map(c => X.indexOf(c).toChar))
    println(sortedList.mkString)
  }

  def findMultiple = {
    io.StdIn.readLine.split(" ").map(_.toInt).toList match {
      case List(a: Int, b: Int, c: Int) =>
        println(cal(a, b, c))
      case _ => // do noting
    }

    def cal(A: Int, B: Int, C: Int): Int = {
      val multiC = C * 2
      if (multiC < A) cal(A, B, multiC)
      else if (multiC < B) multiC
      else -1
    }
  }

  def findMultipleOther = {
    io.StdIn.readLine.split(" ").map(_.toInt).toList match {
      case List(a: Int, b: Int, c: Int) =>
        val y = b / c * c
        if (y >= a) println(y)
        else println(-1)
      case _ =>
    }
  }

  def baseK = {
    val K = io.StdIn.readInt()
    println(io.StdIn.readLine().split(" ").map(x => Integer.parseInt(x, K).toLong).foldLeft(1: Long)((acc, x) => acc * x))
  }

  def longSequence = {
    val N = io.StdIn.readInt
    val seq = io.StdIn.readLine.split(" ").map(_.toLong)
    val X = io.StdIn.readLong
    val seqTotal = seq.sum

    val init = X / seqTotal
    val remain = X % seqTotal
    var tempSum: Long = 0
    var i: Int = 0

    while (tempSum <= remain) {
      tempSum += seq(i % N)
      i += 1
    }

    println(init * N + i)

  }

}

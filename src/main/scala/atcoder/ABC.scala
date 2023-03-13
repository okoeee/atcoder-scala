package atcoder

import java.util.Scanner

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

  def seimicMangitudeScales = {
    io.StdIn.readLine.split(" ").map(_.toInt).toList match {
      case List(a: Int, b: Int) =>
        val diff = a - b
        println(Math.pow(32, diff).toLong)
      case _ =>
    }
  }

  def seimicMangitudeScalesOther = {
    val array = io.StdIn.readLine.split(" ").map(_.toInt)
    val a = array(0)
    val b = array(1)
    val diff = a - b
    println(Math.pow(32, diff).toLong)
  }

  def seimicMangitudeScalesOther2 = {
    val sc = new java.util.Scanner(System.in)
    val a = (sc.nextInt - sc.nextInt)
    println(Math.pow(32, a).toLong)
  }

  def bTypo = {
    val S = io.StdIn.readLine
    val T = io.StdIn.readLine
    if (S == T) println("Yes")
    else {
      val changedList = for (i <- 0 to S.length - 2) yield {
        swap(S, i, i + 1)
      }

      if (changedList.contains(T)) println("Yes")
      else println("No")

    }

    def swap(S: String, i: Int, j: Int): String = {
      val arr = S.split("")
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
      arr.mkString
    }

  }

  def bTypeOther = {
    val sc = new java.util.Scanner(System.in)
    val S, T = sc.next

    def swaps = (0 until S.size).sliding(2).map { case Seq(i, j) => swap(S, i, j) }
    def swap(s: String, i: Int, j: Int) = {
      val arr = s.toArray
      arr(i) = s.charAt(j)
      arr(j) = s.charAt(i)
      arr.mkString
    }
    println(if (S == T || swaps.exists(_ == T)) "Yes" else "No")
  }

  def selectMul = {
    val N = io.StdIn.readLine.split("").sortWith(_ > _)

    val x = N.zipWithIndex.filter(v => v._2 % 2 == 0).map(_._1).mkString.toLong
    val y = N.zipWithIndex.filter(v => v._2 % 2 == 1).map(_._1).mkString.toLong

    println(x * y)
  }

  def selectMulOther = {
    val in = new java.util.Scanner(System.in)
    val N = in.next

    def separate(list: List[List[Int]], l: Long, r: Long, diff: Boolean): Long = list match {
      case Nil => l * r
      case List(ll, rr) :: xs =>
        separate(xs, l * 10 + (if (diff) rr else ll), r * 10 + (if (diff) ll else rr), diff || ll > rr)
    }
  }

  def fourDigits = {
    val N = io.StdIn.readLine
    val diff = 4 - N.length
    println((1 to diff).foldLeft("") { (acc, x) => acc + "0" } + N)
  }

  def fourDigitsOther = {
    val N = io.StdIn.readInt
    println("%04d".format(N))
  }

  def failingGrade = {
    val (n, p) = io.StdIn.readLine.split(" ").map(_.toInt) match { case Array(n: Int, p: Int) => (n, p) }
    val count = io.StdIn.readLine.split(" ").count(a => a.toInt < p)
    println(count)
  }

  def filingGrade = {
    val sc = new java.util.Scanner(System.in)
    val N, P = sc.nextInt
    val scores = List.fill(N)(sc.nextInt)
    scores.count(_ < P)
  }

  def swissSystemTournament = {
    val sc = new java.util.Scanner(System.in)
    val N, M = sc.nextInt

    // 初期化
    case class Player(num: Int, var score: Int)
    val gcpMap = Map("G" -> 1, "C" -> 2, "P" -> 3)
    val inputGcpList = List.fill(2 * N)(sc.next) // じゃんけんのリスト
    var playerList = (1 to 2 * N).map(i => Player(i, 0)).toArray // (番号, 勝ち負け)のリスト

    def calc(m: Int) = {
      // ソート
      val sortedList = playerList.sortBy(p => p.score)
      // 対戦リストの作成
      val matchList = sortedList.grouped(2)
      // 対戦
      matchList.foreach { x =>
        val aNum = x(0).num
        val bNum = x(1).num

        // じゃんけんの手を取得
        val aHand = inputGcpList(aNum - 1).charAt(m)
        val bHand = inputGcpList(bNum - 1).charAt(m)

        judgeAndUpdateScore(aNum - 1, aHand, bNum - 1, bHand)
      }

    }

    def judgeAndUpdateScore(aNum: Int, aHand: Char, bNum: Int, bHand: Char) = {
      val gcpNumA = gcpMap.get(aHand.toString)
      val gcpNumB = gcpMap.get(bHand.toString)

      for (
        i <- gcpNumA;
        j <- gcpNumB
      ) {
        val diff = j - i
        // aが勝ち aに
        if (diff == 1 || diff == -2) {
          playerList(aNum).score = playerList(aNum).score - 1
        } else if (diff == -1 || diff == 2) {
          playerList(bNum).score = playerList(bNum).score - 1
        }
      }

    }

    (0 to M - 1).foreach(calc(_))

    playerList.sortBy(p => p.score).foreach(p => println(p.num))

  }

  def exactPrice = {
    val X = io.StdIn.readInt
    if (X == 0) println("No")
    else if (X % 100 == 0) println("Yes")
    else println("No")
  }

}

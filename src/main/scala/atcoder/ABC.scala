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

  def stringShifting = {

    val S = io.StdIn.readLine
    val arrString = S.split("").toList

    def calc(i: Int, resultList: List[List[String]]): List[List[String]] = {
      if (i > S.length - 1) resultList
      else {
        val list = resultList.last
        val head = list.head
        val tail = list.tail
        val newList = tail :+ head
        calc(i + 1, resultList :+ newList)
      }
    }

    val shiftedList = calc(1, List(arrString)).map(_.mkString).sorted
    println(shiftedList.head)
    print(shiftedList.last)

  }

  def stringShiftingOther = {
    val in = new java.util.Scanner(System.in)
    val S = in.next
    val ss = (1 to S.size).map { i => S.drop(i) + S.take(i) }
    println(ss.min)
    println(ss.max)
  }

  def stringShiftingOther2 = {
    val sc = new java.util.Scanner(System.in)
    val s = sc.next

    def ans(tmp: String, list: List[String]): List[String] = {
      tmp.tail + tmp.head match {
        case str if str == s => s +: list
        case str => ans(str, str +: list)
      }
    }
  }

  def doukasen = {
    val N = io.StdIn.readInt
    val abList = List.fill(N)(io.StdIn.readLine.split(" ").map(_.toDouble))
    val tList = abList.map { case Array(a: Double, b: Double) => a / b }
    val time = tList.sum / 2

    def calc(i: Int, tempT: Double, tempD: Double): Double = {
      val t = tempT + tList(i)
      val d = tempD + abList(i)(0)
      if (t >= time) tempD + (time - tempT) * abList(i)(1)
      else {
        calc(i + 1, t, d)
      }
    }

    val result = calc(0, 0, 0)
    println(result)
  }

  def doukasenOther = {
    val in = new java.util.Scanner(System.in)
    val N = in.nextInt
    val fuses = List.fill(N)((in.nextDouble, in.nextDouble))
    val half = fuses.map { case (a, b) => a / b }.sum / 2

    def search(fs: List[(Double, Double)], t: Double, x: Double): Double = fs match {
      case (a, b) :: xs =>
        val tt = a / b
        if (tt + t >= half) x + b * (half - t)
        else search(xs, t + tt, x + a)
      case _ => 0.0
    }
  }

  def tires = {
    val S = io.StdIn.readLine
    println({
      if (S.endsWith("er")) "er"
      else if (S.endsWith("ist")) "ist"
    })
  }

  def mongeness = {
    val Array(h, w) = io.StdIn.readLine.split(" ").map(_.toInt)
    val A = List.fill(h)(io.StdIn.readLine.split(" ").map(_.toInt))

    val b =
      for (
        i1 <- 0 to w - 2;
        i2 <- i1 + 1 to w - 1;
        j1 <- 0 to h - 2;
        j2 <- j1 + 1 to h - 1
      ) yield {
        val r = A(j1)(i1) + A(j2)(i2) <= A(j1)(i2) + A(j2)(i1)
        r
      }

    println({
      if (b.contains(false)) "No"
      else "Yes"
    })

  }

  def triangle = {
    val n = io.StdIn.readInt
    val areaResult = List.fill(n)(io.StdIn.readLine.split(" ").map(_.toInt)).combinations(3).map { c =>
      val originB = Array(c(1)(0) - c(0)(0), c(1)(1) - c(0)(1))
      val originC = Array(c(2)(0) - c(0)(0), c(2)(1) - c(0)(1))
      Math.abs(originB(0) * originC(1) - originC(0) * originB(1)) / 2
    }
    println(areaResult.count(_ > 0))
  }

  def triangleOther = {
    val N = io.StdIn.readInt
    val dots = List.fill(N)(io.StdIn.readLine.split(" ").map(_.toInt))
    val result = (for (
      i <- 0 to N - 1;
      j <- i + 1 to N - 1;
      k <- j + 1 to N - 1
      if (Math.abs((dots(i)(0) - dots(k)(0)) * (dots(j)(1) - dots(k)(1)) - (dots(j)(0) - dots(k)(0)) * (dots(i)(1) - dots(k)(1))) > 0)
    ) yield 1).sum
    println(result)
  }

  def minMaxPair = {
    val N = io.StdIn.readInt
    val list = io.StdIn.readLine.split(" ").map(_.toInt)
    val result = (for (
      i <- 0 to N - 1;
      j <- i + 1 to N - 1
      if (Math.min(list(i), list(j)) == i + 1 && Math.max(list(i), list(j)) == j + 1)
    ) yield 1).sum
    println(result)
  }

  def minMaxPairOther = {
    val N = io.StdIn.readInt
    val list = -1 +: io.StdIn.readLine.split(" ").map(_.toInt)

    val orderdCount = list.zipWithIndex.filter { case (v, i) => v == i }.size
    val ans = orderdCount * (orderdCount - 1) / 2

    val a = (for (i <- 1 to N) yield {
      val j = list(i)
      if (i < j && list(j) == i) 1 else 0
    }).sum

    println(ans + a)
  }

  def fullHouse = {
    val input = io.StdIn.readLine.split(" ").map(_.toInt)
    println({
      if (input.toSet.size == 2 && input.filter(p => p == input(0)).length >= 2) "Yes" else "No"
    })
  }

  def fullHouseOther = {
    val input = io.StdIn.readLine.split(" ").map(_.toInt).sorted
    println({
      if ((input(0) == input(2) && input(3) == input(4)) || (input(0) == input(1) && input(2) == input(4))) "Yes" else "No"
    })
  }

  def ancestor = {
    val N = io.StdIn.readInt
    val p = -1 +: io.StdIn.readLine.split(" ").map(_.toInt)

    var k = N - 1
    var n = 0
    while (k != 0) {
      k = p(k)
      k = k - 1
      n = n + 1
    }
    println(n)
  }

  def ancestorOther = {
    val N = io.StdIn.readInt
    val p = 0 +: 0 +: io.StdIn.readLine.split(" ").map(_.toInt)

    var crr = N
    var cnt = 0
    while (crr != 1) {
      crr = p(crr)
      cnt = cnt + 1
    }
    println(cnt)
  }

  def monotonicallyIncreasing = {
    val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)
    val l = (m to 1 by -1).foldLeft(List[Int]())((acc, x) => x +: acc)
    l.combinations(n).foreach(v => println(v.mkString(" ")))
  }

  def leftRightOperation = {
    val Array(n, l, r) = io.StdIn.readLine.split(" ").map(_.toInt)
    val p = io.StdIn.readLine.split(" ").map(_.toInt)
    val op = p.clone
    var initR = p.sum

    def opLeft(i: Int, list: Array[Int], preSum: Int): Int = {
      if (i > list.length - 1) preSum
      else {
        list(i) = l
        val newPreSum = if (list.sum < preSum) list.sum else preSum
        opLeft(i + 1, list, newPreSum)
      }
    }
    def opRight(i: Int, list: Array[Int], preSum: Int): Int = {
      if (i < 0) preSum
      else {
        list(i) = r
        val newPreSum = if (list.sum < preSum) list.sum else preSum
        opRight(i - 1, list, newPreSum)
      }
    }

    val resultL = opLeft(0, p, initR)
    val resultR = opRight(p.length - 1, p, initR)
    val onlyR = opRight(op.length - 1, op, initR)

    println(resultL min resultR min onlyR)

  }

  def atcoderSubstr = {
    val a = "atcoder"
    val Array(l, r) = io.StdIn.readLine.split(" ").map(_.toInt)
    println(a.substring(l - 1, r))
  }

  def niceGrid = {
    val Array(r, c) = io.StdIn.readLine.split(" ").map(_.toInt)
    println(if (Math.max(Math.abs(r - 8), Math.abs(c - 8)) % 2 == 1) "black" else "white")
  }

  def matrixReducing = {
    val Array(h1, w1) = io.StdIn.readLine.split(" ").map(_.toInt)
    val A = List.fill(h1)(io.StdIn.readLine.split(" ").map(_.toInt)).toArray
    val Array(h2, w2) = io.StdIn.readLine.split(" ").map(_.toInt)
    val B = List.fill(h2)(io.StdIn.readLine.split(" ").map(_.toInt)).toArray

    val a = A.map { line =>
      // Bの要素をループで
      val index =
        for (
          i <- 0 to h2 - 1;
          j <- 0 to w2 - 1
        ) yield line.indexWhere(_ == B(i)(j))
      index
    }

    a.foreach(p => println(p.mkString(",")))

  }

  def matrixReducingOther = {
    val Array(h1, w1) = io.StdIn.readLine.split(" ").map(_.toInt)
    val A = List.fill(h1)(io.StdIn.readLine.split(" ").map(_.toInt)).toArray
    val Array(h2, w2) = io.StdIn.readLine.split(" ").map(_.toInt)
    val B = List.fill(h2)(io.StdIn.readLine.split(" ").map(_.toInt)).toArray

    val as =
      for (
        selectH <- (0 to h1).combinations(h2);
        selectW <- (0 to w1).combinations(w2)
      ) yield {
        for (
          i <- 0 to h2;
          j <- 0 to w2
          if (B(i)(j) != A(selectH(i))(selectW(j)))
        ) yield false
      }
    as.foreach(p => p.mkString(","))

  }

  def probablyEnglish = {
    val N = io.StdIn.readInt
    val words = io.StdIn.readLine.split(" ")
    val target = Set("and", "not", "that", "the", "you")
    println({
      if (words.exists(target.contains)) "Yes" else "No"
    })
  }

  def probablyEnglishOther = {
    val N = io.StdIn.readInt
    val words = io.StdIn.readLine.split(" ")
    val target = Set("and", "not", "that", "the", "you")
    println({
      if (words.exists(p => target.contains(p))) "Yes" else "No"

    })
  }

  def bombs = {
    val Array(r, c) = io.StdIn.readLine.split(" ").map(_.toInt)
    val b = List.fill(r)(io.StdIn.readLine.toArray)

    def isIn(x: Int, y: Int): Boolean = x >= 0 && x < r && y >= 0 && y < c

    for {
      x <- 0 until r
      y <- 0 until c
      if (b(x)(y).isDigit)
    } {
      val p = b(x)(y).asDigit
      for {
        dx <- -p to p
        dy <- -p to p
        if Math.abs(dx) + Math.abs(dy) <= p
        nx = x + dx
        ny = y + dy
        if isIn(nx, ny)
      } {
        b(nx)(ny) = '.'
      }
    }

    b.foreach(r => println(r.mkString))
  }

  def bombsOther = {
    val Array(r, c) = io.StdIn.readLine.split(" ").map(_.toInt)
    val b = List.fill(r)(io.StdIn.readLine.toArray)

    def isIn(x: Int, y: Int): Boolean = x >= 0 && x < r && y >= 0 && y < c

    val bombs = for {
      x <- 0 until r
      y <- 0 until c
      if b(x)(y).isDigit
    } yield (x, y, b(x)(y).asDigit)

    for {
      (x, y, p) <- bombs
      dx <- -p to p
      dy <- -p to p
      if Math.abs(dx) + Math.abs(dy) <= p
      nx = x + dx
      ny = y + dy
      if isIn(nx, ny)
    } {
      b(nx)(ny) = '.'
    }

    b.foreach(r => println(r.mkString))
  }

  def bombsOtherOfm = {
    val Array(r, c) = io.StdIn.readLine.split(" ").map(_.toInt)
    val B = Array.fill(r)(io.StdIn.readLine.toArray)

    // 爆弾の位置を求める
    val bombs = for {
      i <- 0 to r - 1
      j <- 0 to c - 1
      if B(i)(j).isDigit
    } yield (i, j, B(i)(j) - '0')

    // 爆発させ2次元配列を変更
    for {
      (x, y, p) <- bombs
      dx <- -p to p
      dy <- -p to p
      if Math.abs(dx) + Math.abs(dy) <= p // マンハッタン距離の制限
      if x + dx >= 0 && x + dx <= r - 1 && y + dy >= 0 && y + dy <= c - 1 // 枠からはみ出ないように
    } {
      val nx = x + dx
      val ny = y + dy
      B(nx)(ny) = '.'
    }

    B.foreach(p => println(p.mkString))
  }

  def socks = {
    val N = io.StdIn.readInt
    val arr = io.StdIn.readLine.split(" ").map(_.toInt)
    println(arr.groupBy(identity).mapValues(_.length).values.map(_ / 2).sum)
  }

  def socksOther = {
    val N = io.StdIn.readInt
    val r = io.StdIn.readLine.split(" ").map(_.toInt).groupBy(identity).map { case (_, value) => value.length / 2 }.sum
    println(r)
  }

  def threeDaysAgo = {
    val S = io.StdIn.readLine
    val a = for {
      l <- 0 until S.length
      r <- l to S.length
      if (r - l) % 2 == 0 // 文字の長さが偶数のときしか繰り返すことができないため
    } yield {
      val cutS = S.substring(l, r)
      val (a, b) = cutS.splitAt(cutS.length / 2)
      val countA = a.groupBy(identity).mapValues(_.length)
      val countB = b.groupBy(identity).mapValues(_.length)
      if (countA == countB) 1 else 0
    }
    println(a.sum)
  }

  def redoctaSwap = {
    val S = io.StdIn.readLine.split("")
    val target = "atcoder"
    var count = 0

    def swap(i: Int, j: Int) = {
      val temp = S(i)
      S(i) = S(j)
      S(j) = temp
      count = count + 1
    }

    target.zipWithIndex.foreach { case (v, index) =>
      val iInS = S.indexWhere(_ == s"$v")
      val diff = iInS - index
      if (diff > 0) {
        for (i <- iInS until index by -1) {
          swap(i, i - 1)
        }
      } else if (diff < 0) {
        for (i <- index until iInS) {
          swap(i, i + 1)
        }
      }
    }
    println(count)
  }

  def redoctaSwapOther = {
    val S = io.StdIn.readLine
    val target = "atcoder"
    val originM = target.zipWithIndex.toMap
    val input = S.map(v => originM(v))

    val r = (for {
      i <- 0 until target.size
      j <- i until target.size
      if input(i) > input(j)
    } yield 1).sum

    println(r)

  }

  def apple = {
    val Array(x, y, z) = io.StdIn.readLine.split(" ").map(_.toInt)
    if (x > y / 3) {
      val quotient = z / 3
      val remainder = z % 3
      println(quotient * y + remainder * x)
    } else {
      println(z * x)
    }

  }

  def explore = {
    val Array(n, m, t) = io.StdIn.readLine.split(" ").map(_.toInt)
    val A = io.StdIn.readLine.split(" ").map(_.toInt)
    val X = List.fill(m) {
      val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
      (x, y)
    }

    var mutableT = t

    def calc(i: Int): String = {
      if (i == n) "Yes"
      else {
        X.find(innerList => innerList._1 == i) foreach { x => mutableT = mutableT + x._2 }
        val needT = mutableT - A(i - 1)
        if (needT <= 0) "No"
        else {
          calc(i + 1)
        }
      }
    }
    println(calc(1))
  }

  def exploreOther = {
    val Array(_, m, t) = io.StdIn.readLine.split(" ").map(_.toLong)
    val A = io.StdIn.readLine.split(" ").map(_.toLong)
    (0 until m.toInt).foreach { i =>
      val Array(x, y) = io.StdIn.readLine.split(" ").map(_.toLong)
      A(x.toInt - 1) -= y
    }
    println(if (A.scanLeft(t)((acc, x) => acc - x).forall(_ > 0)) "Yes" else "No")
  }

  def beltConveyorRecursiveVer = {
    val Array(h, w) = io.StdIn.readLine.split(" ").map(_.toInt)
    val G = Array.fill(h)(io.StdIn.readLine.split(""))
    def calc(i: Int, j: Int, b: Int): Either[Int, (Int, Int)] = {
      if (b == 1 && i == 1 && j == 1) Left(-1)
      else {
        G(i - 1)(j - 1) match {
          case "U" if i != 1 => calc(i - 1, j, 1)
          case "D" if i != h => calc(i + 1, j, 1)
          case "L" if j != 1 => calc(i, j - 1, 1)
          case "R" if j != w => calc(i, j + 1, 1)
          case _ => Right(i, j)
        }
      }
    }
    calc(1, 1, 0) match {
      case Right(x) => println(s"${x._1} ${x._2}")
      case Left(x) => println(x)
    }
  }

  def irohaAndHaiku = {
    val Array(_, p, q, r) = io.StdIn.readLine.split(" ").map(_.toLong)
    val A = io.StdIn.readLine.split(" ").map(_.toLong)
    val cSum = A.scanLeft(0L)(_ + _).toSet

    var a = "No"
    cSum.foreach { v =>
      if (cSum(v + p) && cSum(v + p + q) && cSum(v + p + q + r)) a = "Yes"
    }

    println(a)
  }

  def irohaAndHaikuOther = {
    val Array(_, p, q, r) = io.StdIn.readLine.split(" ").map(_.toLong)
    val A = io.StdIn.readLine.split(" ").map(_.toLong)
    val cSum = A.scanLeft(0L)(_ + _).toSet

    println(if (cSum.exists { v => cSum(v + p) && cSum(v + p + q) && cSum(v + p + q + r) }) "Yes" else "No")

  }

  def middleLetter = {
    val S = io.StdIn.readLine
    println(S.charAt(S.length / 2))
  }

  def moduloNumber = {
    val N = io.StdIn.readLong
    val divider = 998244353L
    var a = N % divider
    if (a < 0) a += divider
    println(a)
  }

  def saturday = {
    val S = io.StdIn.readLine
    val weekDay = Array("Friday", "Thursday", "Wednesday", "Tuesday", "Monday")
    println(weekDay.indexOf(S) + 1)
  }

  def split = {
    val arr = 0 +: io.StdIn.readLine.split("").map(_.toInt)
    if (arr(1) == 1) println("No")
    else {
      val eArr = Array.ofDim[Boolean](7)
      eArr(0) = arr(7) > 0
      eArr(1) = arr(4) > 0
      eArr(2) = arr(8) + arr(2) > 0
      eArr(3) = arr(5) + arr(1) > 0
      eArr(4) = arr(3) + arr(9) > 0
      eArr(5) = arr(6) > 0
      eArr(6) = arr(10) > 0
      val rArr = for {
        i <- 0 until eArr.length - 2
        j <- i + 1 until eArr.length - 1
        k <- j + 1 until eArr.length
        if (eArr(i) == true && eArr(j) == false && eArr(k) == true)
      } yield true
      println({
        if (rArr.contains(true)) "Yes" else "No"
      })
    }
  }

  def indexA = {
    val Array(n, m) = io.StdIn.readLine.split(" ").map(_.toInt)
    val A = io.StdIn.readLine.split(" ").map(_.toInt)

    val r = (for (i <- 0 to A.length + 1 - m) yield {
      val slicedA = A.slice(i, i + m)
      slicedA.zipWithIndex.map { case (v, i) => (i + 1) * v }.sum
    }).max

    println(r)
  }

  def indexAOther = {
    val Array(_, m) = io.StdIn.readLine.split(" ").map(_.toInt)
    val A = io.StdIn.readLine.split(" ").map(_.toLong)

    val accA = A.scanLeft(0L)(_ + _)
    val S1 = A.slice(0, m).zipWithIndex.map { case (v, i) => (i + 1) * v }.sum
    val S2List = (0 until A.length - m).scanLeft(S1) { case (acc, x) =>
      acc - (accA(x + m) - accA(x)) + m * A(x + m)
    }
    val r = S2List.max
    println(r)
  }

  def fiveIntegers = {
    println(io.StdIn.readLine.split(" ").map(_.toInt).toSet.size)
  }

  def prefix = {
    val S = io.StdIn.readLine
    val T = io.StdIn.readLine
    println(if (S.length <= T.length && S.zipWithIndex.forall { case (p, i) => p == T(i) }) "Yes" else "No")
  }

  def prefixOther = {
    val S = io.StdIn.readLine
    val T = io.StdIn.readLine
    println(if (T.startsWith(S)) "Yes" else "No")
  }

  def chineseRestaurant = {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt
    val p = Seq.fill(n)(sc.nextInt)
    val ans = p.zipWithIndex.map { case (p_i, i) =>
      Seq((p_i - i + n) % n, (p_i - i - 1 + n) % n, (p_i - i + 1 + n) % n)
    }
    val ans1 = ans.flatten.groupBy(identity).values.map(_.size).max
    println(ans1)
  }

  def anywayTakahashi = {
    val Array(a, b, c, d) = io.StdIn.readLine.split(" ").map(_.toInt)
    println((a + b) * (c - d))
    println("Takahashi")
  }

  def rectangleDetection = {
    val S = Array.fill(10)(io.StdIn.readLine.toArray)
    val ans = S.zipWithIndex.filter { case (arr: Array[Char], i) => arr.contains('#') }.map { case (_, i) => i + 1 }
    val (a, b) = (ans.min, ans.max)
    val (c, d) = (S(a - 1).indexWhere(_ == '#') + 1, S(a - 1).lastIndexWhere(_ == '#') + 1)
    println(a, b, c, d)
  }
  def rectangleDetectionOther = {
    val S = Array.fill(10)(io.StdIn.readLine.toArray)
    val zipS = S.zipWithIndex
    val a = zipS.indexWhere { case (arr, i) => arr.contains('#') } + 1
    val b = zipS.lastIndexWhere { case (arr, i) => arr.contains('#') } + 1
    val c = S(a - 1).indexWhere(_ == '#') + 1
    val d = S(a - 1).lastIndexWhere(_ == '#') + 1
    println(s"""
        |$a $b
        |$c $d
        |""".stripMargin)
  }

  def submask = {
    val N = io.StdIn.readLong
    val nb = N.toBinaryString
    val n = nb.length
    var i = 0
    while (i <= N) {
      val ib = s"%0${n}d".format(BigInt(i.toBinaryString))
      if (ib.zipWithIndex.filter { case (c, i) => c == '1' }.forall { case (elem, i) => elem == nb(i) }) println(i)
      i += 1
    }
  }

  def submaskOther = {
    val N = io.StdIn.readLong
    var ans = Vector[Long](0L)
    for (i <- 0 to 59) {
      if ((N & (1L << i)) != 0) {
        for (e <- ans) ans = ans :+ (e | (1L << i))
      }
    }
    println(ans.mkString("\n"))
  }

  def oneTwoFourTest = {
    val Array(a, b) = io.StdIn.readLine.split(" ").map(_.toInt)
    def returnSeparatedNum(x: Int): List[Int] = x match {
      case 0 => List(0)
      case 1 => List(1)
      case 2 => List(2)
      case 3 => List(1, 2)
      case 4 => List(4)
      case 5 => List(1, 4)
      case 6 => List(2, 4)
      case 7 => List(1, 2, 4)
    }
    println((returnSeparatedNum(a) ++ returnSeparatedNum(b)).toSet.sum)
  }

  def hammer = {
    val sc = new java.util.Scanner(System.in)
    var x = sc.nextInt
    var y = sc.nextInt
    var z = sc.nextInt
    if (x < 0) {
      x = -x
      y = -y
      z = -z
    }
    println(
      if (y > 0 && y < x)
        if (z < y)
          if (z > 0) x
          else Math.abs(z) * 2 + x
        else -1
      else x
    )
  }

  def simplePath = {
    val Array(n, x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
    val uv = Array.fill(n - 1)(io.StdIn.readLine.split(" ").map(_.toInt))

    def calc() = {
      var rArr = Array[Int]()
      val r = uv.find(arr => arr.contains(x))
      if (r.exists(arr => arr.contains(y))) "hoge"
    }
  }

  def DFS = {
    // 入力値の受け取り
    val Array(n, x, y) = io.StdIn.readLine.split(" ").map(_.toInt)
    val uv = Array(-1) +: Array.fill(n)(Array.empty[Int])
    (0 until n - 1).foreach { i =>
      io.StdIn.readLine.split(" ").map(_.toInt) match {
        case Array(x, y) =>
          uv(x) = uv(x) :+ y
          uv(y) = uv(y) :+ x
      }
    }

    var ans = scala.collection.mutable.ListBuffer.empty[Int]

    def DFS(now: Int, pre: Int): Unit = {
      ans += now
      uv(now).foreach { to =>
        if (to != pre) {
          DFS(to, now)
          ans += now
        }
      }
    }

    DFS(1, -1)
    println(ans.mkString(" "))
  }

  def simplePathOther1 = {
    import scala.io.StdIn._
    // 入力値の受け取り
    val Array(n, x, y) = readLine.split(" ").map(_.toInt)
    val edges = Array.fill(n - 1)(readLine().split(" ").map(_.toInt))

    val graph = Array.fill(n + 1)(List.empty[Int])

    // グラフを作成
    edges.foreach { case Array(u, v) =>
      graph(u) = v :: graph(u)
      graph(v) = u :: graph(v)
    }

    def dfs(v: Int, p: Int, path: List[Int]): List[Int] = {
      if (v == y) path :+ v
      else graph(v).filter(_ != p).flatMap(u => dfs(u, v, path :+ v)).takeWhile(_ != y)
    }

    val path = dfs(x, -1, List.empty)

    println(path.mkString(" "))
  }

  def a484558 = {
    val N = io.StdIn.readInt
    println("%02X".format(N))
  }

  def maintainMultipleSequences = {
    import io.StdIn._
    val Array(n, q) = readLine.split(" ").map(_.toInt)
    val ll = Array.fill(n)(readLine.split(" ").map(_.toInt))
    val ss = Array.fill(q)(readLine.split(" ").map(_.toInt))

    val rl = ss.map { case Array(s, t) => ll(s - 1)(t) }
    rl.foreach(println(_))
  }

  def manga = {
    val N = io.StdIn.readInt
    val list = io.StdIn.readLine.split(" ").map(_.toInt).toList

    def calc(preNum: Int, i: Int, list: List[Int]): Int = {
      if (preNum + 1 == list(i)) calc(preNum + 1, i + 1, list)
      else {
        val (last, lastSecond) = (list.last, list(list.length - 2))
        if (lastSecond <= preNum) preNum
        else {
          val dropList = list.dropRight(2)
          calc(preNum + 1, i + 1, (dropList :+ preNum + 1).sorted)
        }
      }
    }

    val r = calc(0, 0, list)
    println(r)

  }

  def integerSum = {
    val N = io.StdIn.readInt
    println(io.StdIn.readLine.split(" ").map(_.toInt).sum)
  }

  def everyoneIsFriends = {
    import scala.io.StdIn.readLine
    val Array(n, m) = readLine.split(" ").map(_.toInt)
    val arr = Array.fill(m)(readLine.split(" ").tail.map(_.toInt).combinations(2).toSet)
    val r = (1 to n).toSet.subsets(2).toList.forall(elem => arr.contains(elem))
    println(if(r) "Yes" else "No")
  }

}

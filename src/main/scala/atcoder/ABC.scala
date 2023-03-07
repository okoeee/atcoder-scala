package atcoder

object ABC {

  def bitWiseExclusiveOr = {

    val in = new java.util.Scanner(System.in)
    val A, B = in.nextInt

    println("input value")
    io.StdIn.readLine.split(" ").map(_.toInt).toList match {
      case head :: tail =>
        println(head ^ tail(0))
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

}

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

}

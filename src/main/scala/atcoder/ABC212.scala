package atcoder

object ABC212 {
  def Alloy = {
    println("input value")
    val list = io.StdIn.readLine.split(" ").map(_.toInt)
    val a = list(0)
    val b = list(1)
    if (a > 0 && b == 0) println("Gold")
    else if (b > 0 && a == 0) println("Silver")
    else println("Alloy")
  }

  def weakPassword = {
    println("input value")
    val list = io.StdIn.readLine.split("").map(_.toInt)
    val calList = list.zipWithIndex.map { case (x, i) => (x - i + 3) % 10 }

    if (list.forall(_ == list.head) | !calList.map(v => v == calList.head).exists(_ == false)) {
      println("Weak")
    } else {
      println("Strong")
    }
  }
}

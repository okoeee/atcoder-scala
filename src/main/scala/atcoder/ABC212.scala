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

  def minDifference = {
    println("input value")
    import scala.math._
    val numList = io.StdIn.readLine.split(" ").map(_.toInt)
    val numOfB = numList.last
    val listA = io.StdIn.readLine.split(" ").map(_.toInt).toSet.toList.sorted
    val listB = io.StdIn.readLine.split(" ").map(_.toInt).toSet.toList.sorted

    def cal(a: Int, listA: List[Int], b: Int, listB: List[Int], min: Int): Int = {
      val diff = abs(a - b)
      if (diff == 0) 0
      else if (min < diff) min
      else cal(listA.head, listA.tail, b, listB, diff)
    }

    println(cal(listA.head, listA, listB.head, listB, Int.MaxValue))

  }

  def queryingMultiset = {
    println("input value")
    val queryNum = io.StdIn.readInt

    def cal(i: Long, boxList: List[Long], recordList: List[Long]): List[Long] = {
      if (i > queryNum) recordList
      else {
        io.StdIn.readLine.split(" ").map(_.toInt).toList match {
          case head :: Nil =>
            val minNum = boxList.min
            val retrievedList = boxList.diff(Seq(minNum))
            println(minNum)
            cal(i + 1, retrievedList, minNum :: recordList)
          case head :: last =>
            if (head == 1) {
              cal(i + 1, last(0) :: boxList, recordList)
            } else {
              cal(i + 1, boxList.map(v => v + last(0)), recordList)
            }
        }
      }
    }

    cal(1, List(), List())
  }

}

package example

import atcoder.{ABC, ABC212, BeginnerContest, BeginnersSelection}

object Main {

  def solution(length: Int, direction: Int, row: Array[Int], col: Array[Int], num: Array[Int]) = {
    val room = Array.fill(length, length)(0)

    val boxes = (0 until row.length).map(i => (row(i), col(i), num(i)))
    boxes.foreach { case (r, c, n) => room(r)(c) = n }

    for {
      (row, col, num) <- boxes
      diff <- 0 to num
      // if row + diff <= length - 1
    } {
      val (diffRow, diffCol, preNum) = direction match {
        case 1 => // 北
          val diffRow = row + diff
          val preNum = room(diffRow)(col)
          (diffRow, col, preNum)
        case 2 => // 東
          val diffCol = col - diff
          val preNum = room(row)(diffCol)
          (row, diffCol, preNum)
        case 3 => // 南
          val diffRow = row - diff
          val preNum = room(diffRow)(col)
          (diffRow, col, preNum)
        case 4 => // 西
          val diffCol = col + diff
          val preNum = room(row)(diffCol)
          (row, diffCol, preNum)
      }

      if (diff == 0 && preNum == num) room(diffRow)(diffCol) = 0
      else if (preNum == 0) room(diffRow)(diffCol) = 1
      else if (preNum < num + 1 - diff && preNum > 0) room(diffRow)(diffCol) = 1 + preNum

    }

    room.foreach(r => println(r.mkString(" ")))

  }

  def main(args: Array[String]): Unit = {
    val length = 6
    val direction = 1
    val row = Array(0, 2)
    val col = Array(2, 2)
    val num = Array(3, 1)
    solution(length, direction, row, col, num)
    // solution(2, 2, Array(0, 1), Array(1, 1), Array(1, 1))
  }
}

package example

import atcoder.{ABC, ABC212, BeginnerContest, BeginnersSelection}

object Main {

  case class Box(row: Int, col: Int, num: Int)

  private def validateInputs(length: Int, direction: Int, row: Array[Int], col: Array[Int], num: Array[Int]): Either[String, Unit] = {
    if (length < 1 || length > 20) Left("lengthの値が不正です")
    else if (num.max > length) Left("ダンボールの高さが最大値を超えています")
    else if (direction < 1 || direction > 4) Left("directionの値が不正です")
    else if (row.length != col.length || row.length != num.length) Left("row, col, numの配列の数が一致しません")
    else if (row.length > length) Left("row, colの範囲がlengthの値を超えています")
    else Right()
  }

  private def isInRange(length: Int, direction: Int, row: Int, col: Int, diff: Int): Boolean = {
    direction match {
      case 1 => row + diff <= length - 1
      case 2 => col - diff >= 0
      case 3 => row - diff >= 0
      case 4 => col + diff <= length - 1
    }
  }

  private def sortBoxesByDirection(direction: Int, box: IndexedSeq[Box]): IndexedSeq[Box] = {
    direction match {
      case 1 => box.sortWith(_.row < _.row)
      case 2 => box.sortWith(_.col > _.col)
      case 3 => box.sortWith(_.row > _.row)
      case 4 => box.sortWith(_.col < _.col)
    }
  }

  private def solution(length: Int, direction: Int, row: Array[Int], col: Array[Int], num: Array[Int]): Unit = {
    validateInputs(length, direction, row, col, num) match {
      case Left(message) => println(message)
      case Right(_) =>
        val room = Array.fill(length, length)(0)
        val boxes = (0 until row.length).map(i => Box(row(i), col(i), num(i)))
        boxes.foreach { case Box(r, c, n) => room(r)(c) = n }

        for {
          Box(row, col, num) <- sortBoxesByDirection(direction, boxes)
          diff <- 0 to num
          if isInRange(length, direction, row, col, diff)
        } {
          val (diffRow, diffCol, preNum) = direction match {
            case 1 =>
              val diffRow = row + diff
              val preNum = room(diffRow)(col)
              (diffRow, col, preNum)
            case 2 =>
              val diffCol = col - diff
              val preNum = room(row)(diffCol)
              (row, diffCol, preNum)
            case 3 =>
              val diffRow = row - diff
              val preNum = room(diffRow)(col)
              (diffRow, col, preNum)
            case 4 =>
              val diffCol = col + diff
              val preNum = room(row)(diffCol)
              (row, diffCol, preNum)
          }
          if (diff == 0 && preNum == num) room(diffRow)(diffCol) = 0
          else if (preNum == 0) room(diffRow)(diffCol) = 1
          else if (preNum > 0 && preNum < num + 1 - diff && boxes.exists(b => b.row == diffRow && b.col == diffCol)) room(diffRow)(diffCol) = 1 + preNum
        }
        room.foreach(row => println(row.mkString(" ")))
    }
  }

  def main(args: Array[String]): Unit = {
    val length = 6
    val direction = 2
    val row = Array(0, 2)
    val col = Array(2, 2)
    val num = Array(3, 1)
    solution(length, direction, row, col, num)
    // solution(2, 2, Array(0, 1), Array(1, 1), Array(1, 1))
    // solution(2, 4, Array(0, 0), Array(0, 1), Array(2, 1))
  }
}

package problems

object Solution {
  // 実行環境
  // Scala 2.12.8
  // openjdk 11.0.16.1
  // sbt 1.2.7

  case class Box(row: Int, col: Int, num: Int)

  private def validateInputs(length: Int, direction: Int, row: Array[Int], col: Array[Int], num: Array[Int]): Either[String, Unit] = {
    if (length < 1 || length > 20) Left("lengthの値が不正です")
    else if ((row ++ col).exists(_ > length - 1)) Left("rowもしくはcolの値がlengthの値を超えています")
    else if (num.max > length) Left("ダンボールの高さが最大値を超えています")
    else if (direction < 1 || direction > 4) Left("directionの値が不正です")
    else if (row.length != col.length || row.length != num.length) Left("row, col, numの配列の数が一致しません")
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

  def solution(length: Int, direction: Int, row: Array[Int], col: Array[Int], num: Array[Int]): Unit = {
    validateInputs(length, direction, row, col, num) match {
      case Left(message) => println(message)
      case Right(_) =>
        // 2次元配列を0で初期化
        val room = Array.fill(length, length)(0)
        // 処理をする際、分かりやすくなるようにケースクラスに代入
        val boxes = row.indices.map(i => Box(row(i), col(i), num(i)))
        // 影の高さを算出する際、もとから存在するダンボールの高さの値が必要なため、roomに代入
        boxes.foreach { case Box(r, c, n) => room(r)(c) = n }

        // ダンボールがある位置とその高さをもとに影の高さを算出し、2次元配列に代入
        for {
          Box(row, col, num) <- sortBoxesByDirection(direction, boxes)
          diff <- 0 to num
          if isInRange(length, direction, row, col, diff)
        } {
          val (diffRow, diffCol) = direction match {
            case 1 => (row + diff, col)
            case 2 => (row, col - diff)
            case 3 => (row - diff, col)
            case 4 => (row, col + diff)
          }
          val preNum = room(diffRow)(diffCol)
          if (diff == 0 && preNum == num) room(diffRow)(diffCol) = 0
          else if (preNum == 0) room(diffRow)(diffCol) = 1
          else if (preNum < num + 1 - diff && boxes.exists(b => b.row == diffRow && b.col == diffCol)) room(diffRow)(diffCol) = 1 + preNum
        }

        room.foreach(row => println(row.mkString(" ")))
    }
  }
}

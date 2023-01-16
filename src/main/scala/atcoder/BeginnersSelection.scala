package atcoder

object BeginnersSelection {

  def practiceA(): Unit = {
    import java.util.Scanner
    val src = new Scanner(System.in)
    val a = src.nextInt()
    val b = src.nextInt()
    val c = src.nextInt()
    val d = src.next

    val sumData = a + b + c

    println(sumData, d)
  }

  def ABC086A(): Unit = {
    import scala.io.StdIn.readLine
    val calculatedVal = readLine.split(" ").foldLeft(1)((x,y) => x.toInt * y.toInt)
    val isEven = calculatedVal % 2 == 0
    if(isEven) {
      println("Even")
    } else {
      println("Odd")
    }

  }

  def ABC081A: Unit = {
    import scala.io.StdIn.readLine
    val inputList = readLine.split("").count(s => s.toInt == 1)
    println(inputList)
  }

  def ABC081B: Unit = {
    import java.util.Scanner
    val src = new Scanner(System.in)
    val _ = src.nextLine()
    val secondLine = src.nextLine()

    val secondLineList = secondLine.split(" ").map{_.toInt}

    def count(c: Int, tails: Array[Int]): Int = {
      if (tails.count(s => s % 2 == 1) > 0) {
        c
      } else {
        val plusC = c + 1
        val divided = tails.map(i => i / 2)
        count(plusC, divided)
      }
    }

    val counts = count(0, secondLineList)
    println(counts)
  }

  def ABC087B: Unit = {
    import java.util.Scanner
    val src = new Scanner(System.in)
    val numOf500 = src.nextInt
    val numOf100 = src.nextInt
    val numOf50 = src.nextInt
    val numOfSum = src.nextInt
    val countList = for {
      n500 <- 0 to numOf500
      n100 <- 0 to numOf100
      n50 <- 0 to numOf50
    } yield {
      val amountOfCalc = (500 * n500) + (100 * n100) + (50 * n50)
      if(numOfSum == amountOfCalc) {
        1
      } else {
        0
      }
    }
    val count = countList.count(n => n == 1)
    println(count)
  }

  def ABC083B: Unit = {
    import java.util.Scanner
    val src = new Scanner(System.in)
    val maxNum = src.nextInt
    val minOfSum = src.nextInt
    val maxOfSum = src.nextInt
    var sumOfNum = 0
    for(n <- 1 to maxNum) {
      val list = n.toString.split("")
      val result = list.foldLeft(0){(x,y) => x.toInt + y.toInt}
      val term = result >= minOfSum && result <= maxOfSum
      if(term) {
        sumOfNum += n
      }
    }
    println(sumOfNum)
  }

  def ABC088B: Unit = {
    val _ = io.StdIn.readInt
    val l = io.StdIn.readLine().split(" ").map(_.toInt)
    val a = l.sorted(Ordering.Int.reverse).zipWithIndex.map(p => {
      if(p._2 % 2 == 0) (p._1)
      else -(p._1)
    })
    println(a.sum)
  }

  def ABC085B: Unit = {
    val N = io.StdIn.readInt
    val n = for(i <- 1 to N) yield io.StdIn.readInt
    val l = n.sorted(Ordering.Int).foldLeft((0, 0)) {(x, y) =>
      if(x._2 < y) {
        (x._1 + 1, y)
      } else {
        (x._1, y)
      }
    }
    println(l._1)
  }

  def ABC085B_ref: Unit = {
    println("入力してください")
    val N = readInt
    var t = Set[Int]()
    for (e <- (1 to N)) {
      t = t + readInt
      println(s"t = $t")
    }
    println(t.size)
  }

  def ABC085B_ref_arrange: Unit = {
    val N = io.StdIn.readInt
    val a = (for(i <- 1 to N) yield io.StdIn.readInt).toSet.size
    println(a)
  }

  def ABC085C: Unit = {
    val Array(numOfBills, amountOfSum) = io.StdIn.readLine().split(" ").map(_.toInt)
    for(
      ix <- 0 to numOfBills;
      iy <- 0 to numOfBills - ix
    ) yield {
      val x = ix
      val y = iy
      val z = numOfBills - x - y
      val isMuchAmount = 1000*x + 5000*y + 10000*z == amountOfSum
      if(isMuchAmount) {
        println(s"$z $y $x")
        sys.exit()
      }
    }
    println("-1 -1 -1")
  }

}

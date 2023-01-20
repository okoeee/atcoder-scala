package atcoder

object BeginnerContest {
  def ABC285_A: Unit = {
    println("Input value")
    val list = io.StdIn.readLine().split(" ").map(_.toInt)
    val (a, b) = (list(0), list(1))
    if(a/2==b || a*2==b || a*2+1==b) {
      println("Yes")
    } else {
      println("No")
    }
  }

  def ABC285_B: Unit = {
    println("input value")
    val numOfString = io.StdIn.readInt
    val inputList = io.StdIn.readLine.split("")
    val a = for(i <- 1 to numOfString-1) yield {
      var b = 0
      for (k <- 1 to numOfString - i) yield {
        if (inputList(k - 1) == inputList(k - 1 + i)) {
          b = k-1
          println(b)
        } else b = k
      }
      b
    }
    //println(a)
  }

}

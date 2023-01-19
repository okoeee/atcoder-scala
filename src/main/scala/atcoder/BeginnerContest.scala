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
}

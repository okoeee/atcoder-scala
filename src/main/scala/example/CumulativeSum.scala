package example

object CumulativeSum {
  val list = List(1,2,3,4,5)
  val cumulativeSum: List[Int] = list.scanLeft(0)(_ + _)
}

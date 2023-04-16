package sort

object Sort {

  def quickSort(seq: Seq[Int]): Seq[Int] = {
    if (seq.length <= 1) seq
    else {
      val pivot = seq(seq.length / 2)
      quickSort(seq.filter(_ < pivot)) ++ seq.filter(_ == pivot) ++ quickSort(seq.filter(pivot < _))
    }
  }

  def quickSortOther(seq: Seq[Int]): Seq[Int] = {
    if (seq.length <= 1) seq
    else {
      val pivot = scala.util.Random.nextInt(seq.length)
      val (less, equal, greater) = seq.foldLeft(List[Int](), List[Int](), List[Int]()) { case ((less, equal, greater), elem) =>
        if (elem < pivot) (elem :: less, equal, greater)
        else if (elem == pivot) (less, elem :: equal, greater)
        else (less, equal, elem :: greater)
      }
      quickSort(less) ++ equal ++ quickSort(greater)
    }
  }

  def mergeSort(seq: Seq[Int]): Seq[Int] = {

    def merge(left: Seq[Int], right: Seq[Int]): Seq[Int] = (left, right) match {
      case (Nil, _) => right
      case (_, Nil) => left
      case (lHead :: lTail, rHead :: rTail) =>
        if (lHead < rHead) lHead +: merge(lTail, right)
        else rHead +: merge(left, rTail)
    }

    val n = seq.length / 2
    if (n == 0) seq
    else {
      val (left, right) = seq.splitAt(n)
      merge(mergeSort(left), mergeSort(right))
    }
  }

}

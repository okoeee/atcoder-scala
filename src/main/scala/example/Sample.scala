package example

object Sample {

  def reverse[T](list: List[T]): List[T] = {
    list.foldLeft(Nil: List[T]) { (acc, x) =>
      x +: acc
    }
  }

}

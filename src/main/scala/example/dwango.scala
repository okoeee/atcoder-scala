package example

object dwango {

  // 変数束縛パターン
  def p1 = {
    val list = List(List("A"), List("B", "C"))
    list match {
      case List(a @ List("A"), x) =>
        println(a)
        println(x)
      case _ => println("nothing")
    }
  }

  // 中置パターン
  def p2 = {
    val list = List("A", "B", "C")
    list match {
      case "A" :: b :: c :: _ =>
        println(b, c)
      case _ =>
        println("nothing")
    }
  }

}

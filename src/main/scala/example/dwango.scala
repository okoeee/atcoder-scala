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

  def p3 = {
    val obj = List("a")
    // パターンマッチにおいて、型変数を使用した場合、正しくパターンマッチが行われない
    obj match {
      case v: List[Int] => println("List[Int]") // ここの行が実行される
      case v: List[String] => println("List[String]")
    }
  }

  def pq1 = {
    for (i <- 1 until 1000) {
      val s = new scala.util.Random(new java.security.SecureRandom()).alphanumeric.take(5).toList match {
        case List(a, b, c, d, e) => List(a, b, c, d, a).mkString
      }
      println(s)
    }
  }

  class Point(val x: Int, val y: Int) {
    def sum: Int = x + y
  }
  val p = new Point(2, 2)
  val px = p.x

  class SubPoint(_x: Int, _y: Int) {
    val x = _x
    val y = _y
  }
  val sp = new SubPoint(1, 2)

  // トレイト
  trait Human {
    val name: String
    val age: Int
    def display() = println(name)
  }

  class Employee(val name: String, val age: Int, expiration: Int) extends Human

}

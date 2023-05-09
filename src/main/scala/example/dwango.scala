package example

object Dwango {

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
    val z = _x + _y
  }
  val sp = new SubPoint(1, 2)

  // トレイト
  trait Human {
    val name: String
    val age: Int
    def display() = println(name)
  }

  class Employee(val name: String, val age: Int, expiration: Int) extends Human

  // クラス練習問題
  // 3次元座標のクラス
  class Point3D(val x: Int, val y: Int, val z: Int)

  // Shapeクラスの継承
  abstract class Shape {
    def area: Double
  }

  class Rectangle(w: Double, h: Double) extends Shape {
    override def area: Double = {
      w * h
    }
  }

  class Circle(r: Double) extends Shape {
    override def area: Double = {
      r * r * Math.PI
    }
  }

  // オブジェクト
  object Point {
    def apply(x: Int, y: Int): Point = new Point(x, y)
  }

  Point(2, 3)

  // トレイト
  trait TraitA {
    val name: String
    def printName = println(name)
  }

  class ClassA(val name: String) extends TraitA

  object ObjectA {
    def init = {
      val a = new ClassA("dwango")
      a.printName
    }
  }

  // 関数
  import scala.io.Source
  def withFile[A](filename: String)(f: Source => A): A = {
    val s = Source.fromFile(filename)
    try {
      f(s)
    } finally {
      s.close
    }
  }
  def printFile(fileName: String): Unit = {
    withFile("hoge") { file =>
      file.getLines.foreach(println)
    }
  }

  // コレクションライブラリ
  def swapArray[T](arr: Array[T])(i: Int, j: Int) = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }

  def joinByComma(start: Int, end: Int): String = {
    (start to end).mkString(",")
  }

  def reverse[T](list: List[T]): List[T] = {
    list.foldLeft(List[T]()) { case (acc, elem) => elem +: acc }
  }

  def mkString[T](list: List[T])(sep: String): String = {
    list.foldLeft("")((acc, elem) => acc + sep + elem.toString).tail
  }

  def mkStringOther[T](list: List[T])(sep: String): String = list match {
    case Nil => ""
    case x :: xs => xs.foldLeft(x.toString)((x, y) => x + sep + y)
  }

  def map[T, U](list: List[T])(f: T => U): List[U] = {
    list.foldLeft(List[U]()) { (acc, elem) => f(elem) :: acc }.reverse
  }

  def filter[T](list: List[T])(f: T => Boolean): List[T] = {
    list.foldLeft(List[T]()) { (acc, elem) => if (f(elem)) elem +: acc else acc }.reverse
  }

  def find[T](list: List[T])(f: T => Boolean): Option[T] = {
    def calc(i: Int): Option[T] = {
      if (i > list.length) None
      else {
        if (f(list(i))) Some(list(i))
        else calc(i + 1)
      }
    }
    calc(0)
  }

  def findOfListVer[T](list: List[T])(f: T => Boolean): Option[T] = list match {
    case x :: xs if f(x) => Some(x)
    case x :: xs => find(xs)(f)
    case _ => None
  }

  // エラー処理
  sealed trait LoginError
  case object InvalidPassword extends LoginError
  case object UserNotFound extends LoginError
  case object PasswordLocked extends LoginError

  case class User(id: Long, name: String, password: String)

  object LoginService {
    def login(name: String, password: String): Either[LoginError, User] = ???
  }

  LoginService.login(name = "dwango", password = "password") match {
    case Right(user) => println(s"id: ${user.id}")
    case Left(InvalidPassword) => println(s"Invalid Password")
  }

  // implicit
  def implicitSample = {
    implicit def intToBoolean(arg: Int): Boolean = arg != 0
    if (1) {
      println("1は真なり")
    }
  }

  // Scala2.10 以前の書き方
  class RichStringOld(val src: String) {
    def smile: String = src + ":-)"
  }

  implicit def enrichString(arg: String): RichString = new RichString(arg)

  // Scala 2.10 以降の書き方
  implicit class RichString(val src: String) {
    def smile(src: String) = src + ":-)"
  }

  // implicit parameter (文脈引き渡し)
  case class Connection(dbName: String, password: String)
  def readRecordsFromTable(columnName: String, tableName: String, connection: Connection) = ???
  // 上記のmethodを書きかえ後
  def rewritedReadRecordsFromTable(columnName: String, tableName: String)(implicit connection: Connection) = ???

  // Listの中身を足すmethod
  trait Additive[A] {
    def zero: A
    def plus(a: A, b: A): A
  }

  object StringAdditive extends Additive[String] {
    def zero: String = ""
    def plus(a: String, b: String): String = a + b
  }

  object IntAdditive extends Additive[Int] {
    def zero: Int = 0
    def plus(a: Int, b: Int): Int = a + b
  }

  def sum[A](list: List[A])(a: Additive[A]) = list.foldLeft(a.zero)((x, y) => a.plus(x, y))

}

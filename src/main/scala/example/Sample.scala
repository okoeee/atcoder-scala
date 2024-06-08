package example

object Sample {

  def parseShow() = ???

  class Hoge(val a: Int, val b: Int)
  val hoge = new Hoge(1, 2)

  case class Foo(a: Int, b: Int)
  val foo = Foo(1, 2)

  // コンストラクタに修飾子をつけることができる
  class Test1 private (a: Int, b: Int)
  object Test1 {
    def applyTest(a: Int, b: Int) = new Test1(a, b)
  }
//  val test1 = Test1(1, 2) // compileでおちる
  val test2 = Test1.applyTest(1, 2)

  // 抽象型
  trait Figure {
    type T
    def area: T
  }
  case class Rectangle(a: Int, b: Int) extends Figure {
    type T = Int
    def area: T = a * b
  }

  abstract class Animal {
    def name: String
  }
  case class Cat(name: String) extends Animal
  case class Dog(name: String) extends Animal

  // 反変
  abstract class Printer[-A] {
    def print(value: A): Unit
  }
  class AnimalPrinter extends Printer[Animal] {
    def print(animal: Animal): Unit =
      println("The animal's name is: " + animal.name)
  }

  class CatPrinter extends Printer[Cat] {
    def print(cat: Cat): Unit =
      println("The cat's name is: " + cat.name)
  }

  new AnimalPrinter().print(Dog("hoge"))
  new CatPrinter().print(Cat("hoge"))

  sealed abstract class Furniture
  case class Couch(name: String) extends Furniture
  case class Chair(name: String) extends Furniture

  def findPlaceToSit(piece: Furniture): String = piece match {
    case a: Couch if a.name == "hoge" => ""
//    case b: Chair => ""
  }

  // パターンマッチ x String interpolation
  // 正規表現を書かずとも文字列の補完ができる
//  def authenticate(request: Request): AuthenticationResult = {
//    request.getHeader("Authorization") match {
//      case Some(s"Bearer $token") if verify(token) => Success(decode(token))
//      case _                                       => Failure
//    }
//  }

}

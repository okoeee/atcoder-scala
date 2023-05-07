package example

object PatternMatch {

  sealed abstract class DayOfWeek
  case object Sunday extends DayOfWeek
  case object Monday extends DayOfWeek
  case object Tuesday extends DayOfWeek
  case object Wednesday extends DayOfWeek
  case object Thursday extends DayOfWeek
  case object Friday extends DayOfWeek
  case object Saturday extends DayOfWeek

  val x: DayOfWeek = Sunday

  x match {
    case Sunday => 1
    case Monday => 2
    case Tuesday => 3
    case Wednesday => 4
    case Thursday => 5
    case Friday => 6
    case Saturday => 7
  }

  def nextDayOfWeek(d: DayOfWeek): DayOfWeek = {
    val preDOW = d match {
      case Sunday => 1
      case Monday => 2
      case Tuesday => 3
      case Wednesday => 4
      case Thursday => 5
      case Friday => 6
      case Saturday => 7
    }
    (preDOW + 1) % 7 match {
      case 1 => Sunday
      case 2 => Monday
      case 3 => Tuesday
      case 4 => Wednesday
      case 5 => Thursday
      case 6 => Friday
      case 7 => Saturday
    }
  }

  def nextDayOfWeekOther(d: DayOfWeek): DayOfWeek = {
    d match {
      case Sunday => Monday
      case Monday => Tuesday
      case Tuesday => Wednesday
      case Wednesday => Thursday
      case Thursday => Friday
      case Friday => Saturday
      case Saturday => Monday
    }
  }

  sealed abstract class Tree
  case class Branch(value: Int, left: Tree, right: Tree) extends Tree
  case object Empty extends Tree

  val tree = Branch(1, Branch(2, Empty, Empty), Branch(3, Empty, Empty))

  def max(tree: Tree): Int = tree match {
    case Branch(elem, Empty, Empty) => elem
    case Branch(elem, Empty, right) =>
      val x = max(right)
      if (elem > x) elem else x
    case Branch(elem, left, Empty) =>
      val x = max(left)
      if (elem > x) elem else x
    case Branch(elem, left, right) =>
      val x = max(left)
      val y = max(right)
      if (elem > x) {
        if (elem > y) elem else y
      } else {
        if (x > y) x else y
      }
    case Empty => throw new RuntimeException
  }

  def depth(tree: Tree): Int = tree match {
    case Empty => 0
    case Branch(_, l, r) =>
      val lDepth = depth(l)
      val rDepth = depth(r)
      (if (lDepth > rDepth) lDepth else rDepth) + 1
  }

}

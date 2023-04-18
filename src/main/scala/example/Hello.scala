package example

import atcoder.{ABC, ABC212, BeginnerContest, BeginnersSelection}
import problems.Solution

import scala.concurrent.{Await, future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object Main {

  def main(args: Array[String]): Unit = {

    // println("input value")
    // ABC.split

    println(Sort.reviewQuickSort(Seq(6, 3, 4, 1, 7, 5, 4, 1, 2, 2, 3)))

    Seq(1,2,3).sorted

  }
}

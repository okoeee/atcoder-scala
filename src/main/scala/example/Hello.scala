package example

import atcoder.{ABC, ABC212, BeginnerContest, BeginnersSelection}
import problems.Solution
import sort.Sort

import scala.concurrent.{Await, future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object Main {

  def main(args: Array[String]): Unit = {

    println("input value")
    // ABC.saturday

    val seq = Sort.mergeSort(Seq(8, 33, 1, 6, 2, 6, 22, 3, 5))
    // println(seq)

    println(Sort.myFind(Seq(1, 2, 3, 4, 5), 0))

  }
}

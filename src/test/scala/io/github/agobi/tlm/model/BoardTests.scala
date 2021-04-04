package io.github.agobi.tlm.model

import utest._


object BoardTests extends TestSuite {
  val seed: Int = 0

  private def getKnownCells(b: Board): Seq[((Int, Int), Int)] = {
    b.rows.flatten.collect {
      case (p, Empty(n)) => b.params.getCoordinates(p) -> n
    }
  }

  override def tests: Tests = Tests {
    test("1st test") {
      val b = Board(10, 10, 10, seed)
      List(5 -> 5).foldLeft(b) { case (b, p) =>
        val r = b.guess(b.params.getPosition(p._1, p._2))
        println(getKnownCells(r))
        r
      }
    }

    test("2nd test") {
      val b = Board(10, 10, 10, seed)
      List(6 -> 3).foldLeft(b) { case (b, p) =>
        val r = b.guess(b.params.getPosition(p._1, p._2))
        println(getKnownCells(r))
        r
      }
    }

    test("3rd test") {
      val b = Board(10, 10, 10, seed)
      List(0 -> 0, 3 -> 9).foldLeft(b) { case (b, p) =>
        val r = b.guess(b.params.getPosition(p._1, p._2))
        println(getKnownCells(r))
        r
      }
    }

    test("4th test") {
      val b = Board(10, 10, 10, seed)
      List(3 -> 4).foldLeft(b) { case (b, p) =>
        val r = b.guess(b.params.getPosition(p._1, p._2))
        println(getKnownCells(r))
        r
      }
    }

    test("finds wrong guess") {
      val b = Board(10, 10, 10, 1853568938)
      val bad = List(0 -> 0, 3 -> 6).foldLeft(b) { case (b, p) =>
        val r = b.guess(b.params.getPosition(p._1, p._2))
        println(getKnownCells(r))
        r
      }
      assert(bad.finished.nonEmpty)
    }
  }
}

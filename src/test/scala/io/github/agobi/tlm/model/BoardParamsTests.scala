package io.github.agobi.tlm.model

import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import utest._


object BoardParamsTests extends TestSuite {
  val tests = Tests {
    "withNeighbors" - validXY.mustSatisfy {
      Prop.test("valid", { case (x, y) =>
        val valid = withValidNeighbors(x, y).map((board.params.getPosition _).tupled)
        val result = board.params.withValidNeighbors(board.params.getPosition(x, y))
        result.toSet == valid.toSet
      })
    }
  }

  val board = Board(3, 4, 0)

  def isIndexValid(x: Int, y: Int): Boolean = x >= 0 && x < board.params.xSize && y >= 0 && y < board.params.ySize
  def withValidNeighbors(x: Int, y: Int): IndexedSeq[(Int, Int)] = {
    (-1 to 1) flatMap { dx =>
      val x2 = x + dx
      (-1 to 1) flatMap { dy =>
        val y2 = y + dy
        if (isIndexValid(x2, y2))
          Some(x2 -> y2)
        else None
      }
    }
  }

  val validXY: Gen[(Int, Int)] = {
    for {
      x <- Gen.chooseInt(0, board.params.xSize - 1)
      y <- Gen.chooseInt(0, board.params.ySize - 1)
    } yield x -> y
  }

}

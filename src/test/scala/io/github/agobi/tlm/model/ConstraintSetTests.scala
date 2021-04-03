package io.github.agobi.tlm.model

import io.github.agobi.tlm.util.choose
import utest._


object ConstraintSetTests extends TestSuite {
  override def tests: Tests = Tests {
    val b = BoardParams(9, 9, 9)
    val c = ConstraintSet(b)

    test("9 mines in empty 9x9") {
      val lhs = c.solutionCount
      val rhs = choose(9 * 9, 9)
      assert(lhs == rhs)
    }

    test("9 mines in 9x9, 3 in the top left corner") {
      val lhs = c.add(b.getPosition(0, 0), 3).solutionCount
      val rhs = choose(9 * 9 - 4, 6)
      assert(lhs == rhs)
    }

    test("9 mines in 9x9, 4 in the top left corner") {
      val lhs = c.add(b.getPosition(0, 0), 4).solutionCount
      assert(lhs == 0)
    }

    test("9 mines in 9x9, 4 somewhere") {
      val lhs = c.add(b.getPosition(1, 1), 4).solutionCount
      val rhs = choose(9 * 8, 5) * choose(8, 4)
      assert(lhs == rhs)
    }

    test("9 mines in 9x9, two 1 clues interacting") {
      val lhs = c.add(b.getPosition(1, 1), 1).add(b.getPosition(1, 3), 1).solutionCount
      val rhs =
        choose(9 * 9 - 15, 8) * 3 + // there is one mine between the two clues
        choose(9 * 9 - 15, 7) * 5 * 5
      assert(lhs == rhs)
    }


    test("No solution") {
      val steps = List(55 -> 0, 56 -> 0, 65 -> 0, 74 -> 0, 84 -> 0, 85 -> 0, 96 -> 0, 97 -> 0, 88 -> 0)

      val cs = steps.foldLeft(ConstraintSet(BoardParams(10, 10, 10))) {
        case (cs, (pos, count)) => cs.add(Board.Position(pos), count)
      }
      assert(cs.solutionCount > 0)

      val cs2 = cs.add(Board.Position(89), 1)
      assert(cs2.solutionCount == 0)
    }
  }
}

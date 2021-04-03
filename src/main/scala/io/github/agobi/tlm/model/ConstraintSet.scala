package io.github.agobi.tlm.model

import io.github.agobi.tlm.util.choose


class ConstraintSet(
    params: BoardParams,
    solved: Set[Board.Position] = Set.empty, // cannot be a mine: Empty or neighbour of an Empty(0)
    constraints: Map[Board.Position, Int] = Map.empty
) {

  assert(
    (constraints.keySet -- solved).isEmpty,
    "Constraints must be in solved"
  )
  assert(
    (constraints.filter(_._2 == 0).keySet.flatMap(params.neighbors) -- solved).isEmpty,
    "Neighbors of 0 constraints must be in solved"
  )


  def add(p: Board.Position, c: Int): ConstraintSet = {
    assert(
      !constraints.contains(p),
      "Cannot update an existing constraint!"
    )

    if (c == 0) {
      val neighbours = params.withValidNeighbors(p)
      new ConstraintSet(params, solved ++ neighbours, constraints.updated(p, c))
    } else {
      new ConstraintSet(params, solved + p, constraints.updated(p, c))
    }
  }

  private def createConstraintSet(): (Int, List[(Int, List[Int])]) = {
    val (varMapping, mappedConstraints) =
      constraints.filter(_._2 > 0).foldLeft(Map.empty[Board.Position, Int] -> List.empty[(Int, List[Int])]) {
        case ((varMapping, constraints), (constraintPosition, constraintCondition)) =>
          val (newMapping, mappedVars) = params.neighbors(constraintPosition)
            .filterNot(solved.contains)
            .foldLeft(varMapping -> List.empty[Int]) { case ((varMapping, mappedVars), neighbor) =>
              val (newMapping, idx) = (varMapping.get(neighbor)) match {
                case Some(idx) => varMapping -> idx
                case None      => varMapping.updated(neighbor, varMapping.size) -> varMapping.size
              }

              newMapping -> (idx :: mappedVars)
          }

        newMapping -> ((constraintCondition -> mappedVars) :: constraints)
      }


    (varMapping.size, mappedConstraints)
  }



  private def solveConstraints(maxMines: Int): (Array[Long], Int) = {
    val ret = Array.fill(maxMines + 1)(0L)
    val (size, constraints) = createConstraintSet()
    val vars = Array.fill(size)(false)

    assert(constraints.flatMap(_._2).forall(_ < size))
    assert(constraints.flatMap(_._2).toSet == (0 until size).toSet)
    assert(constraints.map(_._1).forall(_ >= 0))
    assert(constraints.map(_._1).forall(_ < 9))

    def satisfiable: Boolean =
      constraints.forall {
        case (value, neighbors) => neighbors.count(vars(_)) <= value
      }

    def satisfied: Boolean =
      constraints.forall {
        case (value, neighbors) => neighbors.count(vars(_)) == value
      }

    def countSolutions(i: Int, mines: Int): Unit = {
      if (i < vars.length && mines < ret.length - 1) {
        if (satisfiable) {
          countSolutions(i + 1, mines)
          vars(i) = true
          if (satisfiable) {
            countSolutions(i + 1, mines + 1)
          }
          vars(i) = false
        }
      } else {
        if (satisfied) {
          ret(mines) += 1
        }
      }
    }

    countSolutions(0, 0)
    ret -> size
  }


  lazy val solutionCount: Long = {
    val (countedStates, varCount) = solveConstraints(params.minesCount)
    val unconstrained = params.size - solved.size - varCount

    var ret: Long = 0
    for ((cnt, i) <- countedStates.zipWithIndex) {
      ret += choose(unconstrained, params.minesCount - i) * cnt
    }
    ret
  }
}

object ConstraintSet {
  def apply(boardSize: BoardParams): ConstraintSet = new ConstraintSet(boardSize)

  def fromBoard(b: Board): ConstraintSet = {
    (0 until b.params.xSize * b.params.ySize).foldLeft(new ConstraintSet(b.params, Set.empty, Map.empty)) { case (cs, p) =>
      b.board(p) match {
        case Empty(neighbors) => cs.add(Board.Position(p), neighbors)
        case _ => cs
      }
    }
  }
}

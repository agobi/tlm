package io.github.agobi.tlm.model

import io.github.agobi.tlm.util.choose

trait ConstraintSet {
  def solvedMine: Set[Board.Position]
  def solutionCount: Long
  def add(p: Board.Position, c: Int): ConstraintSet

}

object InvalidConstraintSet extends ConstraintSet {
  override def solvedMine: Set[Board.Position] = Set.empty
  override def solutionCount: Long = 0
  override def add(p: Board.Position, c: Int): ConstraintSet = InvalidConstraintSet
}

class ValidConstraintSet(
  params: BoardParams,
  solvedEmpty: Set[Board.Position] = Set.empty, // cannot be a mine: Empty or neighbour of an Empty(0)
  oldSolvedMine: Set[Board.Position] = Set.empty, // must be a mine: constraint solved cannot find a solution without a mine
  constraints: Map[Board.Position, Int] = Map.empty
) extends ConstraintSet {

  assert(
    (constraints.keySet -- solvedEmpty).isEmpty,
    "Constraints must be in solved"
  )
  assert(
    (constraints.filter(_._2 == 0).keySet.flatMap(params.neighbors) -- solvedEmpty).isEmpty,
    "Neighbors of 0 constraints must be in solved"
  )
  assert(
    solvedMine.intersect(solvedEmpty).isEmpty,
    "Empty and Mine should be disjunct"
  )


  def add(p: Board.Position, c: Int): ConstraintSet = {
    assert(
      !constraints.contains(p),
      "Cannot update an existing constraint!"
    )

    val neighbors = params.withValidNeighbors(p)
    if (neighbors.toSet.intersect(solvedMine).isEmpty) {
      if (c == 0) {
        new ValidConstraintSet(params, solvedEmpty ++ neighbors, solvedMine, constraints.updated(p, c))
      } else {
        new ValidConstraintSet(params, solvedEmpty + p, solvedMine, constraints.updated(p, c))
      }
    } else InvalidConstraintSet
  }

  private def createConstraintSet(): (Map[Int, Board.Position], List[(Int, List[Int])]) = {
    val (varMapping, mappedConstraints) =
      constraints.filter(_._2 > 0).foldLeft(Map.empty[Board.Position, Int] -> List.empty[(Int, List[Int])]) {
        case ((varMapping, constraints), (constraintPosition, constraintCondition)) =>
          val (newMapping, mappedVars) = params.neighbors(constraintPosition)
            .filterNot(solvedEmpty.contains)
            .filterNot(oldSolvedMine.contains)
            .foldLeft(varMapping -> List.empty[Int]) { case ((varMapping, mappedVars), neighbor) =>
              val (newMapping, idx) = varMapping.get(neighbor) match {
                case Some(idx) => varMapping -> idx
                case None      => varMapping.updated(neighbor, varMapping.size) -> varMapping.size
              }

              newMapping -> (idx :: mappedVars)
          }
          val neighborMines = params.neighbors(constraintPosition)
            .count(oldSolvedMine.contains)

        newMapping -> ((constraintCondition - neighborMines -> mappedVars) :: constraints)
      }

    val reversed = varMapping.map(_.swap)
    (reversed, mappedConstraints)
  }



  private def solveConstraints(maxMines: Int): (Array[Long], Int, Set[Board.Position]) = {
    val ret = Array.fill(maxMines + 1)(0L)
    val (varMapping, constraints) = createConstraintSet()
    val vars = Array.fill(varMapping.size)(false)
    val hasEmptySolution = Array.fill(varMapping.size)(false)

    assert(constraints.flatMap(_._2).forall(_ < varMapping.size))
    assert(constraints.flatMap(_._2).toSet == (0 until varMapping.size).toSet)
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
          for (i <- vars.indices) {
            if (!vars(i)) hasEmptySolution(i) = true
          }
          ret(mines) += 1
        }
      }
    }

    countSolutions(0, 0)
    val empties = hasEmptySolution.zipWithIndex.filterNot(_._1).map(p => varMapping(p._2)).toSet

    (ret, varMapping.size, empties)
  }


  lazy val (solutionCount, solvedMine): (Long, Set[Board.Position]) = {
    val (countedStates, varCount, mineSet) = solveConstraints(params.minesCount)
    val solvedMine = oldSolvedMine //++ mineSet
    val unconstrained = params.size - solvedEmpty.size - oldSolvedMine.size - varCount

    var ret: Long = 0
    for ((cnt, i) <- countedStates.zipWithIndex) {
      val k = params.minesCount - oldSolvedMine.size - i
      if (k >= 0 && unconstrained >= k)
        ret += choose(unconstrained, k) * cnt
    }
    ret -> solvedMine
  }
}

object ConstraintSet {
  def apply(boardSize: BoardParams): ConstraintSet = new ValidConstraintSet(boardSize)
}

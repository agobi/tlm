package io.github.agobi.tlm.model

import io.github.agobi.tlm.util.choose

class ConstraintSet(
  val params: BoardParams,
  val solvedEmpty: Set[Board.Position] = Set.empty,
  val oldSolvedMine: Set[Board.Position] = Set.empty,
  val constraints: Map[Board.Position, Int] = Map.empty,
  varMapping: Map[Int, Board.Position] = Map.empty,
  mappedConstraints: List[(Int, List[Int])] = List.empty
) {
  /**
   *  Triple of
   *  - Mapping a variable number to a position
   *  - List of constraint: (sum, list of variables)
   *  - Empty set: Positions which must be empty
   */
  private def varAssignment(constraints: Map[Board.Position, Int]): (Map[Int, Board.Position], List[(Int, List[Int])], Set[Board.Position]) = {
    // Drop 0 conditions, create a set of positions where cannot be a mine
    val (relevantConstraints, emptySet) =
      constraints.foldLeft(List.empty[(Int, Board.Position)] -> Set.empty[Board.Position]) {
        case ((constraints, emptySet), (constraintPosition, constraintCondition)) =>
          val neighbors: List[Board.Position] = params.neighbors(constraintPosition)
          val newCondition: Int = constraintCondition - neighbors.count(solvedMine)

          if(newCondition ==  0) {
            (
              constraints,
              (emptySet + constraintPosition) ++ neighbors.filterNot(solvedMine)
            )
          } else {
            (
              (newCondition -> constraintPosition) :: constraints,
              emptySet + constraintPosition
            )
          }
      }

    val (varMapping, mappedConstraints) =
      relevantConstraints.foldLeft(Map.empty[Board.Position, Int] -> List.empty[(Int, List[Int])]) {
        case ((varMapping, constraints), (constraintCondition, constraintPosition)) =>
          val (newMapping, mappedVars) =
            params.neighbors(constraintPosition)
              .filterNot(emptySet.contains)
              .filterNot(solvedMine.contains)
              .foldLeft(varMapping -> List.empty[Int]) { case ((varMapping, mappedVars), neighbor) =>
                val (newMapping, idx) = varMapping.get(neighbor) match {
                  case Some(idx) => varMapping -> idx
                  case None      => varMapping.updated(neighbor, varMapping.size) -> varMapping.size
                }

                newMapping -> (idx :: mappedVars)
              }

          newMapping -> ((constraintCondition -> mappedVars) :: constraints)
      }

    (varMapping.map(_.swap), mappedConstraints, emptySet)
  }

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


  def add(p: Board.Position, c: Int): Option[ConstraintSet] = {
    assert(
      !constraints.contains(p),
      "Cannot update an existing constraint!"
    )

    val newConstraints = constraints.updated(p, c)
    val (varMapping, mappedConstraints, solvedEmpty) = varAssignment(newConstraints)

    if (solvedEmpty.intersect(solvedMine).isEmpty && mappedConstraints.forall(_._1 >= 0)) {
      Some(new ConstraintSet(
        params,
        solvedEmpty,
        solvedMine,
        newConstraints,
        varMapping,
        mappedConstraints
      ))
    } else None
  }

  private def solveConstraints(maxMines: Int): (Array[Long], Set[Board.Position]) = {
    val ret = Array.fill(maxMines + 1)(0L)
    val vars = Array.fill(varMapping.size)(false)
    val hasEmptySolution = Array.fill(varMapping.size)(false)

    assert(mappedConstraints.flatMap(_._2).forall(_ < varMapping.size))
    assert(mappedConstraints.flatMap(_._2).toSet == (0 until varMapping.size).toSet)
    assert(mappedConstraints.map(_._1).forall(_ < 9))

    def satisfiable: Boolean =
      mappedConstraints.forall {
        case (value, neighbors) => neighbors.count(vars(_)) <= value
      }

    def satisfied: Boolean =
      mappedConstraints.forall {
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


    if (mappedConstraints.map(_._1).forall(_ >= 0))
      countSolutions(0, 0)

    val empties = hasEmptySolution.zipWithIndex.filterNot(_._1).map(p => varMapping(p._2)).toSet

    (ret, empties)
  }


  lazy val (solutionCount, solvedMine): (Long, Set[Board.Position]) = {
    val (countedStates, mineSet) = solveConstraints(params.minesCount - oldSolvedMine.size)
    val unconstrained = params.size - solvedEmpty.size - oldSolvedMine.size - varMapping.size

    var ret: Long = 0
    for ((cnt, i) <- countedStates.zipWithIndex) {
      val k = params.minesCount - oldSolvedMine.size - i
      if (k >= 0 && unconstrained >= k)
        ret += choose(unconstrained, k) * cnt
    }

    ret -> (oldSolvedMine ++ mineSet)
  }
}

object ConstraintSet {
  def apply(boardSize: BoardParams): ConstraintSet = new ConstraintSet(boardSize)
}

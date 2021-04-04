package io.github.agobi.tlm.model


import scala.annotation.tailrec
import scala.util.Random


case class Position(p: Int) extends AnyVal


sealed abstract class CellState
case class Unknown(mine: Option[Boolean]) extends CellState
case class Empty(neighbors: Int) extends CellState

sealed trait Finished
final case class Lost(p: Board.Position) extends Finished
case object Win                          extends Finished


final case class Board private (
  params: BoardParams,
  private val cs: ConstraintSet,
  finished: Option[Finished],
  private val revealed: Int,
  private val random: Random
) {

  def this(params: BoardParams) = {
    this(params, ConstraintSet(params), None, 0, new Random(params.seed))
  }

  def rows: IndexedSeq[IndexedSeq[(Board.Position, CellState)]] = {
    (0 until params.xSize) map { x =>
      (0 until params.ySize) map { y =>
        val p = params.getPosition(x, y)
        p -> get(p)
      }
    }
  }


  def get(p: Board.Position): CellState = {
      cs.constraints.get(p).map(Empty)
        .orElse(if (cs.solvedMine(p)) Some(Unknown(Some(true))) else None)
        .orElse(if (cs.solvedEmpty(p)) Some(Unknown(Some(false))) else None)
        .getOrElse(Unknown(None))
  }

  def guess(p: Board.Position): Board = {
    if (finished.isDefined) {
      this
    } else {
      println(s"Guessing: ${params.getCoordinates(p)}")

      val ret: Board = revealCell(p) match {
        case Some((cs, r)) =>
          val newRevealed = revealed + r
          val newFinished =
            if (params.size == newRevealed + params.minesCount) Some(Win)
            else None

          copy(
            cs = cs,
            revealed = newRevealed,
            finished = newFinished
          )

        case None =>
          copy(finished = Some(Lost(p)))
      }

      println(s"Cells to win ${ret.params.size - ret.revealed - ret.params.minesCount}")
      ret
    }
  }

  private def getSolutions(p: Board.Position, cs: ConstraintSet): Option[(Int, ConstraintSet)] = {
    if (p == params.getPosition(3, 6)) {
      println("Yes!")
    }

    var bestSolutions = Vector.empty[(Int, ConstraintSet)]
    var bestCount: Long = 0L
    for (c <- 0 to 8) {
      cs.add(p, c) match {
        case Some(sol) =>
          val count: Long = sol.solutionCount
          if (count > 0) {
            if (count == bestCount) {
              bestSolutions = bestSolutions.appended(c -> sol)
            } else if (count > bestCount) {
              bestCount = count
              bestSolutions = Vector(c -> sol)
            }
          }
        case None =>
      }
    }

    val ret = if (bestSolutions.isEmpty) None
    else Some(bestSolutions(random.nextInt(bestSolutions.size)))

    println(s"Picked best solution ($bestCount): ${ret.map(_._1)} from ${bestSolutions.map(_._1).mkString(", ")} ")
    ret
  }


  private def revealCell(p: Board.Position): Option[(ConstraintSet, Int)] = {
    if (cs.constraints.contains(p)) Some((cs, 0))
    else getSolutions(p, cs).map { case (c, cs) => revealCells(p -> c, cs) }
  }

  @tailrec
  private def revealCells(
    next: (Board.Position, Int),
    cs: ConstraintSet,
    queued: RandomQueue[Board.Position] = RandomQueue.empty[Board.Position](random),
    checked: Set[Board.Position] = Set.empty,
    revealCount: Int = 0
  ): (ConstraintSet, Int) = {
    val (p, cell) = next
    println(s"Revealing cell: $p -> $cell")

    val neighbors = params
      .neighbors(p)
      .filterNot(x => cs.constraints.contains(x) || checked.contains(x))

    val (queue2, checked2) =
      if (cell != 0) queued -> (checked - p)
      else (queued ++ neighbors) -> (checked ++ neighbors - p)


    queue2.headOption match {
      case Some(next) =>
        val (c, cs2) = getSolutions(next, cs).get
        revealCells(next -> c, cs2, queue2.tail, checked2, revealCount + 1)
      case None =>
        (cs, revealCount + 1)
    }
  }
}

object Board {

  def apply(xSize: Int, ySize: Int, mineCount: Int): Board = {
    val seed = Random.nextInt()
    println(s"Creating ${xSize}x${ySize} board with $mineCount mines, seed $seed")
    new Board(BoardParams(xSize, ySize, mineCount, seed))
  }

  def apply(xSize: Int, ySize: Int, mineCount: Int, seed: Int): Board = {
    new Board(BoardParams(xSize, ySize, mineCount, seed))
  }

  case class Position(p: Int) extends AnyVal
  val NoPosition: Position = Position(-1)
}

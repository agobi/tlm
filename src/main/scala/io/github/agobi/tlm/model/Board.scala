package io.github.agobi.tlm.model


import scala.annotation.tailrec
import scala.util.Random


case class Position(p: Int) extends AnyVal


sealed abstract class CellState(val isRevealed: Boolean) {
  def hasMine: Boolean
}
case class Unknown(override val hasMine: Boolean) extends CellState(false)
case class Empty(neighbors: Int) extends CellState(true) {
  override def hasMine: Boolean = false
}

sealed trait Finished
final case class Lost(p: Board.Position) extends Finished
case object Win                          extends Finished


final case class Board private (
  params: BoardParams,
  private [model] val board: Board.BoardArray,
  private val cs: ConstraintSet,
  finished: Option[Finished],
  private val revealed: Int,
  private val random: Random
) {

  def this(params: BoardParams) = {
    this(params, Vector.fill(params.size)(Unknown(false)), ConstraintSet(params), None, 0, new Random(params.seed))
  }

  def rows: IndexedSeq[IndexedSeq[(Board.Position, CellState)]] = {
    (0 until params.xSize) map { x =>
      (0 until params.ySize) map { y =>
        val p = params.getPosition(x, y)
        p -> get(p)
      }
    }
  }


  def get(p: Board.Position): CellState = board(p.p)

  def guess(p: Board.Position): Board = {
    println(s"Guessing: ${params.getCoordinates(p)}")

    val ret: Board = revealCell(p) match {
      case Some((cs, newBoard, r)) =>
        val newRevealed = revealed + r
        val newFinished =
          if (params.size == newRevealed + params.minesCount) Some(Win)
          else None

        copy(
          cs = cs,
          board = newBoard,
          revealed = newRevealed,
          finished = newFinished
        )

      case None =>
        revealAll().copy(finished = Some(Lost(p)))
    }

    require(
      ret.board.collect { case Empty(_) => () }.size == ret.revealed,
      "Revealed count does not match"
    )
    println(s"Cells to win ${ret.params.size - ret.revealed - ret.params.minesCount}")
    ret
  }

  private def getSolutions(p: Board.Position, cs: ConstraintSet): Option[(Int, ConstraintSet)] = {
    var bestSolutions = Vector.empty[(Int, ConstraintSet)]
    var bestCount: Long = 0L
    for (c <- 0 to 8) {
      val sol = cs.add(p, c)
      val count: Long = sol.solutionCount
      if (count > 0) {
        if (count == bestCount) {
          bestSolutions = bestSolutions.appended(c -> sol)
        } else if (count > bestCount) {
          bestCount = count
          bestSolutions = Vector(c -> sol)
        }
      }
    }

    val ret = if (bestSolutions.isEmpty) None
    else Some(bestSolutions(random.nextInt(bestSolutions.size)))

    println(s"Picked best solution ($bestCount): ${ret.map(_._1)} from ${bestSolutions.map(_._1).mkString(", ")} ")
    ret
  }


  private def revealCell(p: Board.Position): Option[(ConstraintSet, Board.BoardArray, Int)] = {
    if (board(p.p).isRevealed) Some((cs, board, 0))
    else getSolutions(p, cs).map { case (c, cs) => revealCells(p -> c, cs, board) }
  }

  @tailrec
  private def revealCells(
    next: (Board.Position, Int),
    cs: ConstraintSet,
    board: Board.BoardArray,
    queued: RandomQueue[Board.Position] = RandomQueue.empty[Board.Position](random),
    checked: Set[Board.Position] = Set.empty,
    revealCount: Int = 0
  ): (ConstraintSet, Board.BoardArray, Int) = {
    val (p, cell) = next
    println(s"Revealing cell: $p -> $cell")

    val board2 = board.updated(p.p, Empty(cell))

    val neighbors = params
      .neighbors(p)
      .filterNot(x => board2(x.p).isRevealed || checked.contains(x))

    val (queue2, checked2) =
      if (cell != 0) queued -> (checked - p)
      else (queued ++ neighbors) -> (checked ++ neighbors - p)


    queue2.headOption match {
      case Some(next) =>
        val (c, cs2) = getSolutions(next, cs).get
        revealCells(next -> c, cs2, board2, queue2.tail, checked2, revealCount + 1)
      case None =>
        (cs, board2, revealCount + 1)
    }
  }

  private def revealAll(): Board = {
    this
//    copy(
//      board = Vector.from(0 until params.size).map { p =>
//        val cell = board(p)
//        if (cell.isRevealed || cell.hasMine) cell
//        else if (getSolutions(Board.Position(p), cs).isEmpty) Unknown(true)
//        else cell
//      },
//      revealed = params.size - params.minesCount
//    )
  }
}

object Board {
  type BoardArray = Vector[CellState]

  def apply(xSize: Int, ySize: Int, mineCount: Int): Board = {
    new Board(BoardParams(xSize, ySize, mineCount))
  }

  def apply(xSize: Int, ySize: Int, mineCount: Int, seed: Int): Board = {
    new Board(BoardParams(xSize, ySize, mineCount, seed))
  }

  case class Position(p: Int) extends AnyVal
  val NoPosition: Position = Position(-1)
}

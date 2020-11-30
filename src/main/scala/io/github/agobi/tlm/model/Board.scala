package io.github.agobi.tlm.model

import io.github.agobi.tlm.model.Board.BoardArray

import scala.annotation.tailrec
import scala.util.Random

sealed abstract class CellState(val isRevealed: Boolean) {
  def hasMine: Boolean
}

case class Unknown(override val hasMine: Boolean) extends CellState(false)
case class Empty(neighbors: Int) extends CellState(true) {
  override def hasMine: Boolean = false
}

sealed trait Finished
final case class Lost(x: Int, y: Int) extends Finished
case object Win                       extends Finished

final case class Board(
  xSize: Int,
  ySize: Int,
  minesCount: Int,
  board: BoardArray,
  finished: Option[Finished] = None,
  revealed: Int = 0
) {

  def get(x: Int, y: Int): CellState = board(x)(y)

  def row(x: Int): Vector[CellState] = board(x)

  def isIndexValid(x: Int, y: Int): Boolean = x >= 0 && x < xSize && y >= 0 && y < ySize

  def guess(x: Int, y: Int): Board = {
    val ret: Board = revealCell(x, y) match {
      case Some((newBoard, r)) =>
        val newRevealed = revealed + r
        val newFinished =
          if (xSize * ySize == newRevealed + minesCount) Some(Win)
          else None

        copy(
          board = newBoard,
          revealed = newRevealed,
          finished = newFinished
        )

      case None =>
        revealAll().copy(finished = Some(Lost(x, y)))
    }

    require(
      ret.board.map(_.collect { case Empty(_) => () }.size).sum == ret.revealed,
      "Revealed count does not match"
    )
    println(s"Cells to win ${ret.xSize * ret.ySize - ret.revealed - ret.minesCount}")
    ret
  }

  private def revealCell(x: Int, y: Int): Option[(BoardArray, Int)] = {
    if (board(x)(y).hasMine) None
    else if (board(x)(y).isRevealed) Some(board -> 0)
    else Some(revealCells(board, Set(x -> y), Set.empty))
  }

  private def revealAll(): Board = {
    copy(
      board = Vector.from(0 until xSize map { x =>
        Vector.from(0 until ySize map { y =>
          val cell = board(x)(y)
          if (cell.isRevealed || cell.hasMine) cell
          else revealOneCell(x, y)._1
        })
      }),
      revealed = xSize * ySize - minesCount
    )
  }

  private def revealOneCell(x: Int, y: Int): (Empty, IndexedSeq[(Int, Int)]) = {
    require(!board(x)(y).isRevealed, "Internal error: already revealed!")
    require(!board(x)(y).hasMine, "Internal error: mine!")

    val neighbors = withValidNeighbors(x, y)
    Empty(neighbors.count { case (x, y) => board(x)(y).hasMine }) -> neighbors
  }

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

  @tailrec
  private def revealCells(
    board: BoardArray,
    queue: Set[(Int, Int)],
    checked: Set[(Int, Int)]
  ): (BoardArray, Int) = {
    queue.headOption match {
      case None => board -> checked.size
      case Some((x, y)) =>

        val (cell, allNeighbors) = revealOneCell(x, y)
        val neighbors = allNeighbors.filterNot(x => queue.contains(x) || checked.contains(x))

        val checked2 = checked + (x -> y)
        val row      = board(x)
        val queue2 = (
          if (cell.neighbors != 0) queue
          else queue ++ neighbors.filterNot { case (x, y) => board(x)(y).isRevealed }
        ) - (x -> y)

        revealCells(board.updated(x, row.updated(y, cell)), queue2, checked2)
    }
  }
}

object Board {
  type BoardArray = Vector[Vector[CellState]]

  def apply(xSize: Int, ySize: Int, mineCount: Int): Board = {
    @tailrec
    def generateXY(mines: Set[(Int, Int)]): (Int, Int) = {
      val x = Random.nextInt(xSize)
      val y = Random.nextInt(ySize)
      if (mines.contains(x -> y)) generateXY(mines)
      else x -> y
    }

    val mines: Set[(Int, Int)] = (1 to mineCount).foldLeft(Set.empty[(Int, Int)]) { (mines, _) =>
      mines + generateXY(mines)
    }

    val map: BoardArray = Vector.from(0 until xSize map { x =>
      Vector.from(0 until ySize map { y =>
        Unknown(mines.contains(x -> y))
      })
    })

    Board(xSize, ySize, mineCount, map)
  }
}

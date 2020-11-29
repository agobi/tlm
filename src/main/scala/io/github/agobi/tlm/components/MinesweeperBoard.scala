package io.github.agobi.tlm.components

import io.github.agobi.tlm.components.MinesweeperCell._
import io.github.agobi.tlm.styles.{DefaultCommonStyle => style}
import japgolly.scalajs.react.vdom.all.onClickCapture.Event
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ScalaComponent, _}
import scalacss.ScalaCssReact._

import scala.annotation.tailrec
import scala.util.Random


object MinesweeperBoard {
  sealed trait Finished
  final case class Lost(x: Int, y: Int) extends Finished
  case object Win                       extends Finished


  type Board = Vector[Vector[CellState]]

  final case class State(
    xSize: Int,
    ySize: Int,
    minesCount: Int,
    board: Board,
    finished: Option[Finished] = None,
    revealed: Int = 0
  ) {
    def isIndexValid(x: Int, y: Int): Boolean = x >= 0 && x < xSize && y >= 0 && y < ySize

    def guess(x: Int, y: Int): State = {
      val ret: State = revealCell(x, y) match {
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
          copy(finished = Some(Lost(x, y)))
      }

      require(
        ret.board.map(_.collect { case Empty(_) => () }.size).sum == ret.revealed,
        "Revealed count does not match"
      )
      println(s"Cells to win ${ret.xSize * ret.ySize - ret.revealed - ret.minesCount}")
      ret
    }

    private def revealCell(x: Int, y: Int): Option[(Board, Int)] = {
      if (board(x)(y).hasMine) None
      else if (board(x)(y).isRevealed) Some(board -> 0)
      else Some(revealCell(board, Set(x -> y), Set.empty))
    }

    @tailrec
    private def revealCell(
      board: Board,
      queue: Set[(Int, Int)],
      checked: Set[(Int, Int)]
    ): (Vector[Vector[CellState]], Int) = {
      queue.headOption match {
        case None => board -> checked.size
        case Some((x, y)) =>
          require(!board(x)(y).isRevealed, "Internal error: already revealed!")
          require(!board(x)(y).hasMine, "Internal error: mine!")

          val neighbors = (-1 to 1) flatMap { dx =>
            val x2 = x + dx
            (-1 to 1) flatMap { dy =>
              val y2 = y + dy
              if (isIndexValid(x2, y2) && !queue.contains(x2 -> y2) && !checked.contains(x2 -> y2))
                Some(x2 -> y2)
              else None
            }
          }

          val checked2 = checked + (x -> y)
          val row      = board(x)
          val cell     = Empty(neighbors.count { case (x, y) => board(x)(y).hasMine })
          val queue2 = (
            if (cell.neighbors != 0) queue
            else queue ++ neighbors.filterNot { case (x, y) => board(x)(y).isRevealed }
          ) - (x -> y)

          revealCell(board.updated(x, row.updated(y, cell)), queue2, checked2)
      }
    }
  }

  class Backend(bs: BackendScope[Unit, State]) {

    private def addUserStep(state: State, x: Int, y: Int): State = {
      println(s"User clicked on $x:$y")

      val row = state.board(x)
      row(y) match {
        case Empty(_) =>
          state

        case Unknown(_, _) =>
          state.guess(x, y)
      }
    }

    private def clickHandler(x: Int, y: Int): Callback =
      bs.modState(addUserStep(_, x, y))

    def markCell(state: State, x: Int, y: Int): State = {
      println(s"User right clicked on $x:$y")

      val row = state.board(x)
      row(y) match {
        case Empty(_) => state
        case s @ Unknown(_, Unmarked) =>
          state.copy(board = state.board.updated(x, row.updated(y, s.copy(marked = MarkedMine))))
        case s @ Unknown(_, MarkedMine) =>
          state.copy(board = state.board.updated(x, row.updated(y, s.copy(marked = Unmarked))))
      }
    }

    private def contextHandler(x: Int, y: Int)(e: Event): Callback = {
      println(s"User right clicked on $x:$y")
      e.preventDefault()
      bs.modState { s =>
        markCell(s, x, y)
      }
    }

    def render(state: State): VdomTag = {
      val (failX, failY) = state.finished match {
        case Some(Lost(x, y)) => x -> y
        case _                => -1 -> -1
      }

      <.div(
        <.table(
          style.gameTable,
          <.caption(
            state.finished match {
              case None             => "\uD83D\uDE42"
              case Some(Win)        => "\uD83D\uDE0E"
              case Some(Lost(_, _)) => "\u2620\uFE0F️"
            }
          ),
          <.tbody(
            state.board.zipWithIndex.map {
              case (row, x) =>
                <.tr(
                  row.zipWithIndex.map {
                    case (cell, y) =>
                      MinesweeperCell(
                        cell,
                        state.finished.isDefined,
                        clickHandler(x, y),
                        contextHandler(x, y),
                        x == failX && y == failY
                      )
                  }.toTagMod
                )
            }.toTagMod
          )
        ),
        <.div(state.revealed)
      )

    }
  }

  private def initializeState: State = {
    val xSize     = 10
    val ySize     = 10
    val mineCount = 10

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

    val map = Vector.from(0 until xSize map { x =>
      Vector.from(0 until ySize map { y =>
        Unknown(mines.contains(x -> y), marked = Unmarked)
      })
    })

    State(xSize, ySize, mineCount, map)
  }

  val component = {
    ScalaComponent
      .builder[Unit]("Minesweeper>")
      .initialState(initializeState)
      .renderBackend[Backend]
      .build
  }

  def apply() = component()

}

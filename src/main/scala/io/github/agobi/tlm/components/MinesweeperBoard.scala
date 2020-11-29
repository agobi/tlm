package io.github.agobi.tlm.components

import io.github.agobi.tlm.components.MinesweeperCell._
import io.github.agobi.tlm.model._
import io.github.agobi.tlm.styles.{DefaultCommonStyle => style}
import japgolly.scalajs.react.vdom.all.onClickCapture.Event
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ScalaComponent, _}
import monocle.macros.Lenses
import scalacss.ScalaCssReact._

import scala.annotation.tailrec
import scala.util.Random


object MinesweeperBoard {

  @Lenses
  case class State(board: Board, mouseDown: Boolean)

  class Backend(bs: BackendScope[Unit, State]) {

    private def addUserStep(state: State, x: Int, y: Int): State = {
      println(s"User clicked on $x:$y")

      state.board.get(x, y) match {
        case Empty(_) =>
          state
        case Unknown(_, _) =>
          state.copy(board = state.board.guess(x, y))
      }
    }

    private def clickHandler(x: Int, y: Int): Callback =
      bs.modState(addUserStep(_, x, y))

    def markCell(state: State, x: Int, y: Int): State = {
      println(s"User right clicked on $x:$y")

      val row = state.board.row(x)
      row(y) match {
        case Empty(_) => state
        case Unknown(_, Unmarked) =>
          state.copy(board = state.board.update(x, y)(_ => MarkedMine))
        case Unknown(_, MarkedMine) =>
          state.copy(board = state.board.update(x, y)(_ => Unmarked))
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
      val (failX, failY) = state.board.finished match {
        case Some(Lost(x, y)) => x -> y
        case _                => -1 -> -1
      }

      <.div(
        <.table(
          style.gameTable,
          ^.onMouseDown --> bs.modState(State.mouseDown.set(true)),
          ^.onMouseUp --> bs.modState(State.mouseDown.set(false)),
          ^.onMouseLeave --> bs.modState(State.mouseDown.set(false)),
          <.caption(
            state.board.finished match {
              case None if state.mouseDown => "\uD83D\uDE1F"
              case None                    => "\uD83D\uDE42"
              case Some(Win)               => "\uD83D\uDE0E"
              case Some(Lost(_, _))        => "\uD83D\uDE35"
            }
          ),
          <.tbody(
            state.board.board.zipWithIndex.map {
              case (row, x) =>
                <.tr(
                  row.zipWithIndex.map {
                    case (cell, y) =>
                      MinesweeperCell(
                        cell,
                        state.board.finished.isDefined,
                        clickHandler(x, y),
                        contextHandler(x, y),
                        x == failX && y == failY
                      )
                  }.toTagMod
                )
            }.toTagMod
          )
        ),
        <.div(state.board.revealed)
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

    State(Board(xSize, ySize, mineCount, map), false)
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

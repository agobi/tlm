package io.github.agobi.tlm.components

import io.github.agobi.tlm.model._
import io.github.agobi.tlm.styles.{DefaultCommonStyle => style}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ScalaComponent, _}
import monocle.macros.Lenses
import scalacss.ScalaCssReact._


object MinesweeperBoard {
  case class SmileyFaceProps(onClick: Callback, worried: Boolean, finished: Option[Finished])

  val SmileyFace =
    ScalaComponent.builder[SmileyFaceProps]
      .initialState(false)
      .renderPS({ (bs, props, pushed) =>
        <.div(
          style.gameCell(false),
          <.div(
            style.clickableGameCell(pushed),
            ^.onClick --> props.onClick,
            ^.onMouseDown --> bs.setState(true),
            ^.onMouseUp --> bs.setState(false),
            ^.onMouseLeave --> bs.setState(false),
            props.finished match {
              case None if props.worried => "\uD83D\uDE1F"
              case None                  => "\uD83D\uDE42"
              case Some(Win)             => "\uD83D\uDE0E"
              case Some(Lost(_, _))      => "\uD83D\uDE35"
            }
          )
        )
      })
      .build

  def smileyFace(onClick: Callback, worried: Boolean, finished: Option[Finished]) =
    SmileyFace(SmileyFaceProps(onClick, worried, finished))

  @Lenses
  case class State(board: Board, mouseDown: Boolean)

  class Backend(bs: BackendScope[Unit, State]) {
    private def userGuess(x: Int, y: Int): Callback =
      bs.modState(State.board.modify(board => board.get(x, y) match {
        case Empty(_) =>
          board
        case Unknown(_) =>
          board.guess(x, y)
      }))

    private def restartGame(): Callback = {
      bs.setState(initializeState)
    }

    def render(state: State): VdomTag = {
      val (failX, failY) = state.board.finished match {
        case Some(Lost(x, y)) => x -> y
        case _                => -1 -> -1
      }

      <.div(
        <.table(
          style.gameTable,
          <.caption(smileyFace(restartGame(), state.mouseDown, state.board.finished)),
          <.tbody(
            ^.onMouseDown --> bs.modState(State.mouseDown.set(true)),
            ^.onMouseUp --> bs.modState(State.mouseDown.set(false)),
            ^.onMouseLeave --> bs.modState(State.mouseDown.set(false)),
            state.board.board.zipWithIndex.map {
              case (row, x) =>
                <.tr(
                  row.zipWithIndex.map {
                    case (cell, y) =>
                      MinesweeperCell(
                        cell,
                        state.board.finished.isDefined,
                        userGuess(x, y),
                        x == failX && y == failY
                      )
                  }.toTagMod
                )
            }.toTagMod
          )
        )
      )
    }
  }

  private def initializeState: State = {
    val xSize     = 10
    val ySize     = 10
    val mineCount = 10

    State(Board(xSize, ySize, mineCount), mouseDown = false)
  }

  val component = {
    ScalaComponent
      .builder[Unit]("Minesweeper")
      .initialState(initializeState)
      .renderBackend[Backend]
      .build
  }

  def apply() = component()

}

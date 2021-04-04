package io.github.agobi.tlm.components

import io.github.agobi.tlm.components.MinesweeperCell.{Marked, Unmarked}
import io.github.agobi.tlm.model.{Board, _}
import io.github.agobi.tlm.styles.{DefaultCommonStyle => style}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ScalaComponent, _}
import monocle.macros.Lenses
import monocle.function.all._
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
              case Some(Lost(_))         => "\uD83D\uDE35"
            }
          )
        )
      })
      .build

  def smileyFace(onClick: Callback, worried: Boolean, finished: Option[Finished]) =
    SmileyFace(SmileyFaceProps(onClick, worried, finished))

  @Lenses
  case class State(board: Board, mouseDown: Boolean, marked: Vector[Marked])

  class Backend($: BackendScope[Unit, State]) {

    private def userGuess(p: Board.Position): Callback =
      for {
        s <- $.state
        ret <- $.modState(State.board.modify(board => board.get(p) match {
          case Unknown(_) if s.marked(p.p) == Unmarked =>
            board.guess(p)
          case _ =>
            board
        }))
      } yield ret

    private def userGuessNeighbors(p: Board.Position): Callback = {
      for {
        state <- $.state
        neighbors = state.board.params.withValidNeighbors(p).map(userGuess)
        ret <- Callback.sequence(neighbors)
      } yield ret
    }


    private def restartGame(): Callback = {
      $.setState(initializeState)
    }

    def markCell(p: Board.Position): Callback =
      $.modState(State.marked composeOptional index(p.p) modify (_.next))


    def render(state: State): VdomTag = {
      val failPosition: Board.Position = state.board.finished match {
        case Some(Lost(p)) => p
        case _             => Board.NoPosition
      }

      <.div(
        <.div(state.board.params.seed),
        <.table(
          style.gameTable,
          <.caption(smileyFace(restartGame(), state.mouseDown, state.board.finished)),
          <.tbody(
            ^.onMouseDown --> $.modState(State.mouseDown.set(true)),
            ^.onMouseUp --> $.modState(State.mouseDown.set(false)),
            ^.onMouseLeave --> $.modState(State.mouseDown.set(false)),
            state.board.rows.map {
              case row =>
                <.tr(
                  row.map {
                    case (p, cell) =>
                      MinesweeperCell(
                        cell,
                        state.board.finished.isDefined,
                        userGuess(p),
                        userGuessNeighbors(p),
                        markCell(p),
                        p == failPosition,
                        state.marked(p.p)
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

    val marks = Vector.from(0 until xSize * ySize map { _ =>  Unmarked } )
    State(Board(xSize, ySize, mineCount), mouseDown = false, marks)
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

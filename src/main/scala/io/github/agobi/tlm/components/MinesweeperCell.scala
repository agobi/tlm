package io.github.agobi.tlm.components

import io.github.agobi.tlm.model.{CellState, Empty, Unknown}
import io.github.agobi.tlm.styles.{DefaultCommonStyle => style}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.all.onClickCapture.Event
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import monocle.macros.Lenses
import scalacss.ScalaCssReact._


object MinesweeperCell {
  sealed trait Marked
  case object Unmarked   extends Marked
  case object MarkedMine extends Marked

  case class Props(
    state: CellState,
    finished: Boolean,
    onClick: Callback,
    failed: Boolean
  )


  @Lenses
  case class State(
    pushed: Boolean,
    marked: Marked
  )

  class Backend($: BackendScope[Props, State]) {

    def render(state: State, props: Props) = {
      if (props.finished) renderFinalCell(state, props.state, props.failed)
      else renderGameCell(state, props.state, props.onClick)
    }

    private def renderUserMark(marked: Marked, mine: Option[Boolean]) = (marked, mine) match {
      case (Unmarked, Some(true)) =>
        "\uD83D\uDCA3"
      case (Unmarked, _) =>
        "\u00a0"
      case (MarkedMine, Some(false)) =>
        "\u274C"
      case (MarkedMine, _) =>
        "\uD83D\uDEA9"
    }

    private def renderFinalCell(state: State, cell: CellState, failed: Boolean): VdomTag = {
      cell match {
        case Unknown(mine) =>
          <.td(style.gameCell(failed), <.div(renderUserMark(state.marked, Some(mine))))
        case Empty(neighbors) =>
          <.td(style.gameCell(false), <.div(neighbors.toString))
      }
    }


    private def contextHandler(e: Event): Callback = {
      e.preventDefault()
      $.modState(State.marked.modify {
        case Unmarked => MarkedMine
        case MarkedMine => Unmarked
      })
    }


    private def renderGameCell(state: State, cell: CellState, clickHandler: Callback): VdomTag = {
      cell match {
        case Unknown(_) =>
          <.td(
            style.gameCell(false),
            <.div(
              style.clickableGameCell(state.pushed),
              renderUserMark(state.marked, None),
              ^.onClick --> clickHandler,
              ^.onContextMenu ==> contextHandler,
              ^.onMouseDown --> $.modState(State.pushed.set(true)),
              ^.onMouseUp --> $.modState(State.pushed.set(false)),
              ^.onMouseEnter ==> { e => $.modState(State.pushed.set(e.buttons != 0)) },
              ^.onMouseLeave --> $.modState(State.pushed.set(false)),
            )
          )
        case Empty(neighbors) =>
          <.td(style.gameCell(false), <.div(neighbors.toString))
      }
    }
  }

  val component =
    ScalaComponent.builder[Props]
      .initialState(State(pushed = false, Unmarked))
      .renderBackend[Backend]
      .build

  def apply(
    state: CellState,
    finished: Boolean,
    onClick: Callback,
    failed: Boolean
  ) = component(Props(state, finished, onClick, failed))

}

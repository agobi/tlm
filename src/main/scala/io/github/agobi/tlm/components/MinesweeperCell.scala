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
    onMark: (Marked => Marked) => Callback,
    failed: Boolean,
    marked: Marked
  )


  @Lenses
  case class State(
    pushed: Boolean
  )

  class Backend($: BackendScope[Props, State]) {

    def render(state: State, props: Props) = {
      if (props.finished) renderFinalCell(props)
      else renderGameCell(props, state)
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

    private def renderFinalCell(props: Props): VdomTag = {
      props.state match {
        case Unknown(mine) =>
          <.td(style.gameCell(props.failed), <.div(renderUserMark(props.marked, Some(mine))))
        case Empty(neighbors) =>
          <.td(style.gameCell(false), <.div(neighbors.toString))
      }
    }


    private def contextHandler(onMark: (Marked => Marked) => Callback)(e: Event): Callback = {
      e.preventDefault()
      onMark {
        case Unmarked => MarkedMine
        case MarkedMine => Unmarked
      }
    }


    private def renderGameCell(props: Props, state: State): VdomTag = {
      props.state match {
        case Unknown(_) =>
          <.td(
            style.gameCell(false),
            <.div(
              style.clickableGameCell(state.pushed),
              renderUserMark(props.marked, None),
              ^.onClick --> props.onClick,
              ^.onContextMenu ==> contextHandler(props.onMark),
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
      .initialState(State(pushed = false))
      .renderBackend[Backend]
      .build

  def apply(
    state: CellState,
    finished: Boolean,
    onClick: Callback,
    onMark: (Marked => Marked) => Callback,
    failed: Boolean,
    marked: Marked
  ) = component(Props(state, finished, onClick, onMark, failed, marked))

}

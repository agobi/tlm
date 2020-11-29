package io.github.agobi.tlm.components

import io.github.agobi.tlm.styles.{DefaultCommonStyle => style}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.all.onClickCapture.Event
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import scalacss.ScalaCssReact._


object MinesweeperCell {
  sealed trait Marked
  case object Unmarked   extends Marked
  case object MarkedMine extends Marked

  sealed trait CellState {
    def hasMine: Boolean
    def isRevealed: Boolean
  }

  case class Unknown(override val hasMine: Boolean, marked: Marked) extends CellState {
    override def isRevealed: Boolean = false
  }

  case class Empty(neighbors: Int) extends CellState {
    override def hasMine: Boolean    = false
    override def isRevealed: Boolean = true
  }

  case class Props(
    state: CellState,
    finished: Boolean,
    onClick: Callback,
    onContextMenu: Event => Callback,
    failed: Boolean
  )

  class Backend(bs: BackendScope[Props, Boolean]) {

    def render(state: Boolean, props: Props) = {
      if (props.finished) renderFinalCell(props.state, props.failed)
      else renderGameCell(state, props.state, props.onClick, props.onContextMenu)
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

    private def renderFinalCell(cell: CellState, failed: Boolean): VdomTag = {
      cell match {
        case Unknown(mine, marked) =>
          <.td(style.gameCell(failed), <.div(renderUserMark(marked, Some(mine))))
        case Empty(neighbors) =>
          <.td(style.gameCell(false), <.div(neighbors.toString))
      }
    }

    private def renderGameCell(state: Boolean, cell: CellState, clickHandler: Callback, contextHandler: Event => Callback): VdomTag = {
      cell match {
        case Unknown(_, marked) =>
          <.td(
            style.gameCell(false),
            <.div(
              style.clickableGameCell(state),
              renderUserMark(marked, None),
              ^.onClick --> clickHandler,
              ^.onContextMenu ==> contextHandler,
              ^.onMouseDown --> bs.setState(true),
              ^.onMouseUp --> bs.setState(false),
              ^.onMouseEnter ==> { e =>
                bs.setState(e.buttons != 0)
              },
              ^.onMouseLeave --> bs.setState(false)
            )
          )
        case Empty(neighbors) =>
          <.td(style.gameCell(false), <.div(neighbors.toString))
      }
    }
  }

  val component =
    ScalaComponent.builder[Props]
      .initialState(false)
      .renderBackend[Backend]
      .build

  def apply(
    state: CellState,
    finished: Boolean,
    onClick: Callback,
    onContextMenu: Event => Callback,
    failed: Boolean
  ) = component(Props(state, finished, onClick, onContextMenu, failed))

}

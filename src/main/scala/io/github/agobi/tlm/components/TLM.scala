package io.github.agobi.tlm.components

import io.github.agobi.tlm.styles.{DefaultCommonStyle => style}
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^.<
import scalacss.ScalaCssReact._


object TLM {

  class Backend {

    def render() = {
      <.div(
        style.game,
        MinesweeperBoard()
      )
    }
  }

  val component = ScalaComponent
    .builder[Unit]("MinesweeperApp")
    .renderBackend[Backend]
    .build

  def apply() = component()
}

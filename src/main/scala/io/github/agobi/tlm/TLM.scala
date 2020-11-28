package io.github.agobi.tlm

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import io.github.agobi.tlm.styles.{DefaultCommonStyle => style}
import scalacss.ScalaCssReact._


object TLM {
  class Backend {
    def render() = {
      <.div(
        style.game,
        Minesweeper()
      )
    }
  }

  val component = ScalaComponent.builder[Unit]("Tutorial")
    .renderBackend[Backend]
    .build

  def apply() = component()
}

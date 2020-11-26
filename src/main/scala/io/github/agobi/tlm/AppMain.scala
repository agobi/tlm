package io.github.agobi.tlm

import io.github.agobi.tlm.styles.CssSettings._
import io.github.agobi.tlm.styles.DefaultCommonStyle
import org.scalajs.dom
import scalacss.ScalaCssReact._

import scala.scalajs.js.annotation.JSExport

object AppMain {

  @JSExport("app")
  def main(args: Array[String]): Unit = {
    DefaultCommonStyle.addToDocument()

    val root = dom.document.getElementById("root")
    TLM().renderIntoDOM(root)

    ()
  }
}

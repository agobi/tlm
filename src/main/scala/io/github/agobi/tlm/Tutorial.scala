package io.github.agobi.tlm

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.raw.Element

object Tutorial {
  case class State(messages: List[String])

  class Backend(bs: BackendScope[Unit, State]) {
    def render(state: State) = {
      val messages = state.messages.map { msg =>
        <.p(msg)
      }

      <.div(
        <.p("Hello World"),
        <.button("Click me!", ^.onClick --> addClickedMessage()),
        messages.toTagMod
      )
    }

    def addClickedMessage(): CallbackTo[Unit] = bs.modState(
      s => State("You clicked the button!" :: s.messages)
    )
  }

  val component = ScalaComponent.builder[Unit]("Tutorial")
    .initialState(State(Nil))
    .renderBackend[Backend]
    .build

  def apply() = component()

  def setupUI(element: Element): Unit = {
    Tutorial().renderIntoDOM(element)
    ()
  }



  def main(args: Array[String]): Unit = {
    setupUI(dom.document.getElementById("root"))
  }
}

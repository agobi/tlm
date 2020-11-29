package io.github.agobi.tlm.styles

import io.github.agobi.tlm.styles.CssSettings._

import scala.language.postfixOps

class CommonStyle extends StyleSheet.Inline {
  import dsl._

  val game = style(
    font := "18px arial"
  )

  val gameTable = style(
    userSelect.none,
    backgroundColor(grey(0xec)),
    borderCollapse.collapse,
    width.auto,
    marginLeft.auto,
    marginRight.auto,
    marginTop(1 em),
    unsafeChild("caption")(
      borderTop(1 px, solid, grey(0x80)),
      borderLeft(1 px, solid, grey(0x80)),
      borderRight(1 px, solid, grey(0x80)),
      backgroundColor(grey(0xec)),
      padding(0.5 em, `0`, 0.5 em, `0`),
      unsafeChild("div")(
        display.inlineBlock,
        boxSizing.borderBox
      )
    ),
    unsafeChild("td")(
      padding.`0`,
      margin.`0`,
    )
  )

  val gameCell = styleF.bool(failed => styleS(
    border(1 px, solid, grey(0x80)),
    backgroundColor(if (failed) red else inherit),
    verticalAlign.middle,
    textAlign.center,
    cursor.pointer,
    &.hover {
      backgroundColor(grey(0xdc))
    },
    unsafeChild("div")(
      width(1.5 em),
      height(1.5 em),
      lineHeight(1.5 em),
      padding(0 em),
      boxSizing.borderBox,
  )))

  val clickableGameCell = styleF.bool(pushed => styleS(
    backgroundColor(grey(0xc0)),
    if (pushed) border(2 px, inset) else border(2 px, outset),
    borderSpacing.`0`,
    &.hover {
      backgroundColor(grey(0xa0))
    }
  ))

  val neighborDomain = Domain.ofRange(0 to 8)
  val neighborColorMap = Vector(
    rgb(0x00, 0x00, 0x00), // 0
    rgb(0x00, 0x00, 0xff), // 1
    rgb(0x00, 0x81, 0x00), // 2
    rgb(0xff, 0x13, 0x00), // 3
    rgb(0x00, 0x00, 0x83), // 4
    rgb(0x81, 0x05, 0x00), // 5
    rgb(0x2a, 0x94, 0x94), // 6
    rgb(0x00, 0x00, 0x00), // 7
    rgb(0x80, 0x80, 0x80)  // 8
  )

  val neighborCount = styleF(neighborDomain)(neighbors => styleS(
    color(neighborColorMap(neighbors))
  ))
}

object DefaultCommonStyle extends CommonStyle {

}

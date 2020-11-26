package io.github.agobi.tlm.styles

import io.github.agobi.tlm.styles.CssSettings._
import scala.language.postfixOps

class CommonStyle extends StyleSheet.Inline {
  import dsl._

  val game = style(
    unsafeRoot("body") {
      style(
        fontSize(18 px),
        fontFamily :=! "arial"
      )
    }
  )

  val gameTable = style(
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
      padding(0.2 em, `0`, 0.2 em, `0`)
    )
  )

  val gameCell = style(
    border(1 px, solid, grey(0x80)),
    padding.`0`,
    margin.`0`,
    verticalAlign.middle,
    textAlign.center,
    unsafeChild("div")(
      width(1.4 em),
      height(1.4 em),
      lineHeight(1.4 em),
      padding(0 em),
      boxSizing.borderBox
    )
  )

  val clickableGameCell = style(
    backgroundColor(grey(0xc0)),
    border(2 px, outset),
    borderSpacing.`0`,
    cursor.pointer,
    &.hover {
      backgroundColor(grey(0xb0))
    }
  )
}

object DefaultCommonStyle extends CommonStyle

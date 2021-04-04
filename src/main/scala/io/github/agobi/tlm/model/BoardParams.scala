package io.github.agobi.tlm.model

case class BoardParams(xSize: Int, ySize: Int, minesCount: Int, seed: Int) {
  require(xSize > 2 && ySize > 2)

  val size: Int = xSize * ySize

  private [model] def getCoordinates(p: Board.Position): (Int, Int) = {
    p.p / ySize -> p.p % ySize
  }

  private [model] def getPosition(x: Int, y: Int): Board.Position = {
    Board.Position(x * ySize + y)
  }

  private def withValidNeighbors_(p: Int): List[Int] = {
    // p == x * ySize + y
    val validX: List[Int] = if(p < ySize) List(0, ySize)
    else if(p >= (xSize-1) * ySize) List(0, -ySize)
    else List(-ySize, 0, ySize)

    val validY: List[Int] = if(p % ySize == 0) List(0, 1)
    else if(p % ySize == ySize - 1) List(-1, 0)
    else List(-1, 0, 1)

    for {
      x <- validX
      y <- validY
    } yield p + x + y
  }

  def withValidNeighbors(p: Board.Position): List[Board.Position] = {
    withValidNeighbors_(p.p).map(Board.Position)
  }

  def neighbors(p: Board.Position): List[Board.Position] = {
    withValidNeighbors_(p.p).map(Board.Position).filterNot(_ == p)
  }
}

object BoardParams {
  def apply(xSize: Int, ySize: Int, minesCount: Int, seed: Int): BoardParams = {
    new BoardParams(xSize = xSize, ySize = ySize, minesCount = minesCount, seed = seed)
  }
}

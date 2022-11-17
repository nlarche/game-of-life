import scala.util.Try

@main def GameOfLife: Unit = {
  val dead = DeadCell()
  val alive = AliveCell()
  val matrix = Matrix(
    Seq(
      Seq(alive, dead, alive, dead, dead, alive, dead),
      Seq(alive, dead, alive, dead, dead, alive, dead),
      Seq(alive, dead, alive, dead, dead, alive, dead),
      Seq(dead, alive, alive, alive, dead, alive, dead),
      Seq(dead, alive, alive, alive, dead, alive, dead),
      Seq(dead, alive, alive, alive, dead, alive, dead),
      Seq(dead, alive, alive, alive, dead, alive, dead),
      Seq(dead, alive, alive, alive, dead, alive, dead)
    ))
  var m: Matrix = matrix
  for (_ <- 0 to 100) {
    println(m)
    println("----")
    m = computeNextMatrix(m)
  }
}

def computeNextMatrix(matrix: Matrix): Matrix = {
  Matrix(computeAliveNeighbours(matrix).map(c => c.map(cell => {
    cellFactory(cell.cell.computeNextCellState(cell.value))
  })))
}

def computeAliveNeighbours(matrix: Matrix): Seq[Seq[CellNeighbourHood]] = {
  matrix.matrix.zipWithIndex.map { case (col, colIndex) =>
    col.zipWithIndex.map { case (cell, rowIndex) => CellWithPosition(cell, Position(colIndex, rowIndex))
    }
  }.map(cells => cells.map(cellWithPosition =>
    CellNeighbourHood(cellWithPosition.cell, matrix.computeNeighbours(cellWithPosition.position))))
}

case class Matrix(matrix: Seq[Seq[Cell]]) {
  def getNeighbours(col: Int, row: Int): Option[Cell] = Try(matrix(col)(row)).fold(_ => {
    None
  }, cell => Some(cell))

  override def toString: String = matrix.map(m => m.mkString(",")).mkString("\n")


  def computeNeighbours(position: Position): Int = {
    val positions = (Seq(Tuple2(-1, -1), Tuple2(0, 1), Tuple2(0, -1), Tuple2(1, 0), Tuple2(-1, 0), Tuple2(1, 1), Tuple2(1, -1), Tuple2(-1, 1)))
    (for {
      t <- positions
      cell <- getNeighbours(position.col + t._1, position.row + t._2)
    } yield cell match {
      case _: AliveCell => 1
      case _: DeadCell => 0
      case _ => 0
    }).sum
  }
}

case class CellNeighbourHood(cell: Cell, value: Int) {}

case class Position(col: Int, row: Int) {}

case class CellWithPosition(cell: Cell, position: Position) {}

trait Cell(state: State) {
  def computeNextCellState(neighbours: Int): State
}

case class DeadCell() extends Cell(DEAD) {
  override def toString: String = "D"

  def computeNextCellState(neighbours: Int): State = neighbours match
    case x if (x == 3) => ALIVE
    case _ => DEAD
}

case class AliveCell() extends Cell(ALIVE) {
  override def toString: String = "A"

  def computeNextCellState(neighbours: Int): State = neighbours match
    case x if (x < 2) => DEAD
    case x if (x > 3) => DEAD
    case _ => ALIVE
}

def cellFactory(state: State): Cell = state match
  case DEAD => DeadCell()
  case ALIVE => AliveCell()


sealed trait State {}

case object ALIVE extends State

case object DEAD extends State

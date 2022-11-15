import scala.util.Try

class GameOfLife5 extends munit.FunSuite {
  // rules
  // alive => < 2 => dead
  // alive =>  2 || 3  => alive
  // alive =>  > 3  => dead
  // dead => = 3 => alive

  test("[[A, A]] Cells should have [[1, 1]] neighbours") {
    val cell = Cell(ALIVE)
    val cell2 = Cell(ALIVE)
    val matrix = Matrix(Seq(Seq(cell, cell2)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(Seq(CellNeighbourHood(cell, 1), CellNeighbourHood(cell2, 1)))
    assertEquals(obtained, expected)
  }

  test("[[A, A, A]] Cells should have [[1 2 1]] neighbours") {
    val cell = Cell(ALIVE)
    val cell2 = Cell(ALIVE)
    val cell3 = Cell(ALIVE)
    val matrix = Matrix(Seq(Seq(cell, cell2, cell3)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(Seq(CellNeighbourHood(cell, 1), CellNeighbourHood(cell2, 2), CellNeighbourHood(cell3, 1)))
    assertEquals(obtained, expected)
  }

  test("[[D, A, D]] Cells should have [1 0 1] neighbours") {
    val cell = Cell(DEAD)
    val cell2 = Cell(ALIVE)
    val cell3 = Cell(DEAD)
    val matrix = Matrix(Seq(Seq(cell, cell2, cell3)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(Seq(CellNeighbourHood(cell, 1), CellNeighbourHood(cell2, 0), CellNeighbourHood(cell3, 1)))
    assertEquals(obtained, expected)
  }

  test("[[A, A, A]] Cells should have [[1 0 1]] neighbours") {
    val cell = Cell(DEAD)
    val cell2 = Cell(ALIVE)
    val cell3 = Cell(DEAD)
    val matrix = Matrix(Seq(Seq(cell, cell2, cell3)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(Seq(CellNeighbourHood(cell, 1), CellNeighbourHood(cell2, 0), CellNeighbourHood(cell3, 1)))
    assertEquals(obtained, expected)
  }

  test("[[A][A]] Cells should have [[1][1]] neighbours") {
    val cell = Cell(ALIVE)
    val cell2 = Cell(ALIVE)
    val matrix = Matrix(Seq(Seq(cell), Seq(cell2)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(Seq(CellNeighbourHood(cell, 1)), Seq(CellNeighbourHood(cell2, 1)))
    assertEquals(obtained, expected)
  }

  test("[[D, A, D][D, A, D]] Cells should have [[2, 1, 2][2, 1, 2]] neighbours") {
    val cell = Cell(DEAD)
    val cell2 = Cell(ALIVE)
    val cell3 = Cell(DEAD)
    val cell4 = Cell(DEAD)
    val cell5 = Cell(ALIVE)
    val cell6 = Cell(DEAD)
    val matrix = Matrix(Seq(Seq(cell, cell2, cell3), Seq(cell4, cell5, cell6)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(Seq(CellNeighbourHood(cell, 2), CellNeighbourHood(cell2, 1), CellNeighbourHood(cell3, 2)), Seq(CellNeighbourHood(cell4, 2), CellNeighbourHood(cell5, 1), CellNeighbourHood(cell6, 2)))
    assertEquals(obtained, expected)
  }

  test("[[D, A, D][D, A, D][A, A, A]] Cells should have [[2, 1, 2][4, 4, 4][2, 3, 2] neighbours") {
    val cell = Cell(DEAD)
    val cell2 = Cell(ALIVE)
    val cell3 = Cell(DEAD)
    val cell4 = Cell(DEAD)
    val cell5 = Cell(ALIVE)
    val cell6 = Cell(DEAD)
    val cell7 = Cell(ALIVE)
    val cell8 = Cell(ALIVE)
    val cell9 = Cell(ALIVE)
    val matrix = Matrix(Seq(Seq(cell, cell2, cell3), Seq(cell4, cell5, cell6), Seq(cell7, cell8, cell9)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(
      Seq(CellNeighbourHood(cell, 2), CellNeighbourHood(cell2, 1), CellNeighbourHood(cell3, 2)),
      Seq(CellNeighbourHood(cell4, 4), CellNeighbourHood(cell5, 4), CellNeighbourHood(cell6, 4)),
      Seq(CellNeighbourHood(cell7, 2), CellNeighbourHood(cell8, 3), CellNeighbourHood(cell9, 2))
    )
    assertEquals(obtained, expected)
  }

  test("[[D, A, D][D, A, D][A, A, A]] Cells should have [[2, 1, 2][4, 4, 4][2, 3, 2] neighbours") {
    val dead = Cell(DEAD)
    val alive = Cell(ALIVE)
    val matrix = Matrix(Seq(Seq(dead, alive, dead), Seq(dead, alive, dead), Seq(alive, alive, alive)))
    val obtained = computeNextMatrix(matrix)
    val expected = Matrix(Seq(Seq(dead, dead, dead), Seq(dead, dead, dead), Seq(alive, alive, alive)))
    assertEquals(obtained, expected)
  }

  def computeNextMatrix(matrix: Matrix): Matrix ={
    Matrix(computeAliveNeighbours(matrix).map(c => c.map(cell => {
      println(s"${cell.value}: ${cell.cell.state}, ${computeCellState(cell.value)}")
      Cell(computeCellState(cell.value))
    })))
  }

  def computeAliveNeighbours(matrix: Matrix): Seq[Seq[CellNeighbourHood]] = {
    matrix.matrix.zipWithIndex.map { case (row, colIndex) =>
      row.zipWithIndex.map { case (cell, rowIndex) => CellWithPosition(cell, Position(colIndex, rowIndex))
      }
    }.map(cells => cells.map(cellWithPosition =>
      CellNeighbourHood(cellWithPosition.cell, matrix.computeNeighbours(cellWithPosition.position))))
  }

  case class Matrix(matrix: Seq[Seq[Cell]]) {
    def getNeighbours(col: Int, row: Int): Int =
      Try(matrix(col)(row)).toOption match
        case Some(x) => x.map(cell => if (cell.isAlive) 1 else 0)
        case _ => 0;

    def computeNeighbours(position: Position): Int = {
      val a = getNeighbours(position.col, position.row - 1)
      val b = getNeighbours(position.col, position.row + 1)
      val c = getNeighbours(position.col - 1, position.row)
      val d = getNeighbours(position.col + 1, position.row)
      val e = getNeighbours(position.col - 1, position.row - 1)
      val f = getNeighbours(position.col + 1, position.row + 1)
      val g = getNeighbours(position.col - 1, position.row + 1)
      val h = getNeighbours(position.col + 1, position.row - 1)
      a + b + c + d + e + f + g + h
    }
  }

  case class CellNeighbourHood(cell: Cell, value: Int) {}

  case class Position(col: Int, row: Int) {}

  case class CellWithPosition(cell: Cell, position: Position) {}

    case class Cell(state: State) {
    def map[Int](f: (Cell) => Int): Int = {
      f(this)
    }
    def isAlive = state == ALIVE
  }

  sealed trait Population {}
  object UnderPopulated extends Population
  object OverPopulated extends Population
  object WellPopulated extends Population

  sealed trait State{ }
  case object ALIVE extends State
  case object DEAD extends State

  def neighbourHoodFactory(neighbours: Int): Population = neighbours match {
    case x if (x < 2) => UnderPopulated
    case x if (x > 3) => OverPopulated
    case _ => WellPopulated
  }

  def computeCellState(neighbours: Int): State = neighbourHoodFactory(neighbours) match
    case UnderPopulated => DEAD
    case OverPopulated => DEAD
    case _ => ALIVE
}

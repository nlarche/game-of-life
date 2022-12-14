import scala.util.Try

class GameOfLife6 extends munit.FunSuite {
  // rules
  // alive => < 2 => dead
  // alive =>  2 || 3  => alive
  // alive =>  > 3  => dead
  // dead => = 3 => alive

  test("[[A, A]] Cells should have [[1, 1]] neighbours") {
    val cell = AliveCell()
    val cell2 = AliveCell()
    val matrix = Matrix(Seq(Seq(cell, cell2)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(Seq(CellNeighbourHood(cell, 1), CellNeighbourHood(cell2, 1)))
    assertEquals(obtained, expected)
  }

  test("[[A, A, A]] Cells should have [[1 2 1]] neighbours") {
    val cell = AliveCell()
    val cell2 = AliveCell()
    val cell3 = AliveCell()
    val matrix = Matrix(Seq(Seq(cell, cell2, cell3)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(Seq(CellNeighbourHood(cell, 1), CellNeighbourHood(cell2, 2), CellNeighbourHood(cell3, 1)))
    assertEquals(obtained, expected)
  }

  test("[[D, A, D]] Cells should have [1 0 1] neighbours") {
    val cell = DeadCell()
    val cell2 = AliveCell()
    val cell3 = DeadCell()
    val matrix = Matrix(Seq(Seq(cell, cell2, cell3)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(Seq(CellNeighbourHood(cell, 1), CellNeighbourHood(cell2, 0), CellNeighbourHood(cell3, 1)))
    assertEquals(obtained, expected)
  }

  test("[[A, A, A]] Cells should have [[1 0 1]] neighbours") {
    val cell = DeadCell()
    val cell2 = AliveCell()
    val cell3 = DeadCell()
    val matrix = Matrix(Seq(Seq(cell, cell2, cell3)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(Seq(CellNeighbourHood(cell, 1), CellNeighbourHood(cell2, 0), CellNeighbourHood(cell3, 1)))
    assertEquals(obtained, expected)
  }

  test("[[A][A]] Cells should have [[1][1]] neighbours") {
    val cell = AliveCell()
    val cell2 = AliveCell()
    val matrix = Matrix(Seq(Seq(cell), Seq(cell2)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(Seq(CellNeighbourHood(cell, 1)), Seq(CellNeighbourHood(cell2, 1)))
    assertEquals(obtained, expected)
  }

  test("[[D, A, D][D, A, D]] Cells should have [[2, 1, 2][2, 1, 2]] neighbours") {
    val cell = DeadCell()
    val cell2 = AliveCell()
    val cell3 = DeadCell()
    val cell4 = DeadCell()
    val cell5 = AliveCell()
    val cell6 = DeadCell()
    val matrix = Matrix(Seq(Seq(cell, cell2, cell3), Seq(cell4, cell5, cell6)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(Seq(CellNeighbourHood(cell, 2), CellNeighbourHood(cell2, 1), CellNeighbourHood(cell3, 2)), Seq(CellNeighbourHood(cell4, 2), CellNeighbourHood(cell5, 1), CellNeighbourHood(cell6, 2)))
    assertEquals(obtained, expected)
  }

  test("[[D, A, D][D, A, D][A, A, A]] Cells should have [[2, 1, 2][4, 4, 4][2, 3, 2] neighbours") {
    val cell = DeadCell()
    val cell2 = AliveCell()
    val cell3 = DeadCell()
    val cell4 = DeadCell()
    val cell5 = AliveCell()
    val cell6 = DeadCell()
    val cell7 = AliveCell()
    val cell8 = AliveCell()
    val cell9 = AliveCell()
    val matrix = Matrix(Seq(Seq(cell, cell2, cell3), Seq(cell4, cell5, cell6), Seq(cell7, cell8, cell9)))
    val obtained = computeAliveNeighbours(matrix)
    val expected = Seq(
      Seq(CellNeighbourHood(cell, 2), CellNeighbourHood(cell2, 1), CellNeighbourHood(cell3, 2)),
      Seq(CellNeighbourHood(cell4, 4), CellNeighbourHood(cell5, 4), CellNeighbourHood(cell6, 4)),
      Seq(CellNeighbourHood(cell7, 2), CellNeighbourHood(cell8, 3), CellNeighbourHood(cell9, 2))
    )
    assertEquals(obtained, expected)
  }

  test("[[D, A, D][D, A, D][A, A, A]] matrix should become [[D, D, D][D, D, D][A, A, A] after 1 iteration") {
    val dead = DeadCell()
    val alive = AliveCell()
    val matrix = Matrix(Seq(Seq(dead, alive, dead), Seq(dead, alive, dead), Seq(alive, alive, alive)))
    val obtained = computeNextMatrix(matrix)
    val expected = Matrix(Seq(Seq(dead, dead, dead), Seq(dead, dead, dead), Seq(alive, alive, alive)))
    assertEquals(obtained, expected)
  }

  def computeNextMatrix(matrix: Matrix): Matrix ={
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


    def computeNeighbours(position: Position): Int = {
      val positions = (Seq(Tuple2(-1,-1), Tuple2(0,1), Tuple2(0,-1), Tuple2(1,0), Tuple2(-1,0), Tuple2(1,1), Tuple2(1,-1), Tuple2(-1,1)))
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
  case class DeadCell() extends Cell(DEAD){
    def computeNextCellState(neighbours: Int): State = neighbours match
      case x if (x == 3) => ALIVE
      case _ => DEAD
  }
  case class AliveCell() extends Cell(ALIVE){
    def computeNextCellState(neighbours: Int): State = neighbours match
      case x if (x < 2) => DEAD
      case x if (x > 3) => DEAD
      case _ => ALIVE
  }

  def cellFactory(state: State): Cell = state match
    case DEAD => DeadCell()
    case ALIVE => AliveCell()


  sealed trait State{ }
  case object ALIVE extends State
  case object DEAD extends State


}

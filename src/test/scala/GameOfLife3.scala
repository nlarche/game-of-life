import scala.util.Try

class GameOfLife3 extends munit.FunSuite  {
  // rules
  // alive => < 2 => dead
  // alive =>  2 || 3  => alive
  // alive =>  > 3  => dead
  // dead => = 3 => alive

  test("Should be alive with 2") {
    val obtained = computeCellState(2)
    val expected = Right(ALIVE)
    assertEquals(obtained, expected)
  }

  test("Should become dead with 1") {
    val obtained = computeCellState(1)
    val expected = Left(DEAD)
    assertEquals(obtained, expected)
  }

  test("Should become dead with 4") {
    val obtained = computeCellState(4)
    val expected = Left(DEAD)
    assertEquals(obtained, expected)
  }

  test("Alone Cell should not have [0] neighbours") {
    val cell = Cell(ALIVE)
    val obtained = computeAliveNeighbours(Seq(cell))
    val expected = Seq(CellNeighbourHood(cell, 0))
    assertEquals(obtained, expected)
  }

 test("[A, A] Cells should have [1, 1] neighbours") {
    val cell = Cell(ALIVE)
    val cell2 = Cell(ALIVE)
    val obtained = computeAliveNeighbours(Seq(cell, cell2))
    val expected = Seq(CellNeighbourHood(cell, 1), CellNeighbourHood(cell2, 1))
    assertEquals(obtained, expected)
  }

  test("[A, A, A] Cells should have [1 2 1] neighbours") {
    val cell = Cell(ALIVE)
    val cell2 = Cell(ALIVE)
    val cell3 = Cell(ALIVE)
    val obtained = computeAliveNeighbours(Seq(cell, cell2, cell3))
    val expected = Seq(CellNeighbourHood(cell, 1), CellNeighbourHood(cell2, 2), CellNeighbourHood(cell3, 1))
    assertEquals(obtained, expected)
  }

  test("[D, A, D] Cells should have [1 0 1] neighbours") {
    val cell = Cell(DEAD)
    val cell2 = Cell(ALIVE)
    val cell3 = Cell(DEAD)
    val obtained = computeAliveNeighbours(Seq(cell, cell2, cell3))
    val expected = Seq(CellNeighbourHood(cell, 1), CellNeighbourHood(cell2, 0), CellNeighbourHood(cell3, 1))
    assertEquals(obtained, expected)
  }

  def isAliveNeighbour(currentCellIndex: Int)(cell: Cell, index: Int): Boolean = {
    cell.isAlive && Math.abs(index - currentCellIndex) == 1
  }
  def computeAliveNeighbours(row: Seq[Cell]): Seq[CellNeighbourHood] = {
    row.zipWithIndex.map{case (cell, index) =>
      CellNeighbourHood(cell, row.zipWithIndex.filter(isAliveNeighbour(index)).length)}
  }

  case class CellNeighbourHood(cell: Cell, value: Int){}

  case class Cell(state: State){
    def isAlive = state == ALIVE
  }

  sealed trait Population {}
  object UnderPopulated extends Population
  object OverPopulated extends Population
  object WellPopulated extends Population

  sealed trait State
  case object ALIVE extends State
  case object DEAD extends State

  def neighbourHoodFactory(neighbours: Int): Population = neighbours match {
    case x if (x < 2) => UnderPopulated
    case x if (x > 3) => OverPopulated
    case _ => WellPopulated
  }

  def computeCellState(neighbours: Int): Either[State, State] = neighbourHoodFactory(neighbours) match
    case UnderPopulated => Left(DEAD)
    case OverPopulated => Left(DEAD)
    case _ => Right(ALIVE)

}

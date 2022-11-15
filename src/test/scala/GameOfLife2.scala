class GameOfLife2 extends munit.FunSuite  {
  // rules
  // alive => < 2 => dead
  // alive =>  2 || 3  => alive
  // alive =>  > 3  => dead
  // dead => = 3 => alive

  test("Should be alive with 2") {
    val obtained = computeCellState(2)
    val expected = ALIVE
    assertEquals(obtained, expected)
  }

  test("Should become dead with 1") {
    val obtained = computeCellState(1)
    val expected = DEAD
    assertEquals(obtained, expected)
  }

  test("Should become dead with 4") {
    val obtained = computeCellState(4)
    val expected = DEAD
    assertEquals(obtained, expected)
  }

  sealed trait Populated {}
  object UnderPopulated extends Populated
  object OverPopulated extends Populated
  object WellPopulated extends Populated

  sealed trait State
  case object ALIVE extends State
  case object DEAD extends State

  def neighbourHoodFactory(neighbours: Int): Populated = neighbours match {
    case x if (x < 2) => UnderPopulated
    case x if (x > 3) => OverPopulated
    case _ => WellPopulated
  }

  def computeCellState( neighbours: Int): State = neighbourHoodFactory(neighbours) match
    case UnderPopulated => DEAD
    case OverPopulated => DEAD
    case _ => ALIVE

}

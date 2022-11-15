class GameOfLife extends munit.FunSuite  {
  // rules
  // alive => < 2 => dead
  // alive =>  2 || 3  => alive
  // alive =>  > 3  => dead
  // dead => = 3 => alive

  test("Should be alive with 2") {
    val obtained = computeCellState(2)
    val expected = "alive"
    assertEquals(obtained, expected)
  }

  test("Should become dead with 1") {
    val obtained = computeCellState(1)
    val expected = "dead"
    assertEquals(obtained, expected)
  }

  test("Should become dead with 4") {
    val obtained = computeCellState(4)
    val expected = "dead"
    assertEquals(obtained, expected)
  }

  def computeCellState( neighbours: Int): String = neighbours match
    case x if (x < 2) => "dead"
    case x if (x > 3) => "dead"
    case _ => "alive"

}

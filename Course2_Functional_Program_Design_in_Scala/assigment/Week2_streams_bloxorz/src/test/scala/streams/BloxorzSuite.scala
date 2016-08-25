package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class  BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level0 extends SolutionChecker {
    val level =
            """ST
              |oo
              |oo""".stripMargin
    val vector: Vector[Vector[Char]] =
      Vector(level.split("\n").map(str => Vector(str: _*)): _*)
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrainFunction in stringParser") {
    new Level0 {
      val terrainFunc = terrainFunction(vector)
      for{
        i <- (0 to 2)
        j <- (0 to 1)
      } yield assert(terrainFunc(new Pos(i, j)))

      assert(!terrainFunc(new Pos(3, 0)))
      assert(!terrainFunc(new Pos(0, 2)))

    }
  }

  test("findChar in stringParser") {
    new Level0 {
      assert(findChar('S', vector) === new Pos(0, 0))
      assert(findChar('T', vector) === new Pos(0, 1))
    }
  }

	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("neighborsWithHistory level 1") {
    new Level1 {
      val resultNeighborsSet = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      )
      //println(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toList)
      assert(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet === resultNeighborsSet)
    }
  }

  test("newNeighborsOnly level 1") {
    new Level1 {
      val funcResult = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )
      //println(funcResult.toList)
      assert(funcResult.toSet === Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}

package net.egp.puzzle

import org.scalatest.{FlatSpec, Matchers}

class PuzzleSpec extends FlatSpec with Matchers {
  val puzzle = new Puzzle()

  "The rot method" should "rotate" in {
    puzzle.rot("ABC")(Context(0, (0, 1, 2), Seq())) should be("abc")
    puzzle.rot("ABC")(Context(6, (0, 1, 2), Seq())) should be("ghi")
    puzzle.rot("ABC")(Context(26 - 6, (0, 1, 2), Seq())) should be("uvw")
  }

  "findAllWords" should "return a list" in {
    puzzle.findAllWords("podocarp", "PRO")(Context(0, (3, 6, 0), Seq())) should be(List("abcdefg"))
  }

}
//case class Context(rot: Int, columns: (Int, Int, Int), dictList: Seq[String])
// TODO

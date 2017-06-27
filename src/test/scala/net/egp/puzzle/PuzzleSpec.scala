package net.egp.puzzle

import org.scalatest.{FlatSpec, Matchers}

class PuzzleSpec extends FlatSpec with Matchers {
  val puzzle = new Puzzle()

  "The rot method" should "rotate" in {
    puzzle.rot("ABC")(Context(0, (0, 1, 2), Seq())) should be("abc")
    puzzle.rot("ABC")(Context(6, (0, 1, 2), Seq())) should be("ghi")
    puzzle.rot("ABC")(Context(26 - 6, (0, 1, 2), Seq())) should be("uvw")
  }

  "findAllWords for Context(0, (3, 6, 0))" should "return lists" in {
    puzzle.findAllWords("saxhorns")(Context(0, (3, 6, 0), Seq())) should be(List("sighting", "saxhorns"))
    puzzle.findAllWords("safetied")(Context(0, (3, 6, 0), Seq())) should be(List(
      "sweeties", "sweepier", "sweenies", "sureties", "superset", "supermen", "superjet", "superber",
      "streusel", "stresses", "stressed", "streeled", "streeker", "streeked", "streamer", "streamed", "streaker",
      "streaked", "steeples", "steepled", "steelies", "steelier", "squegged", "squeezes", "squeezer", "squeezed",
      "squeegee", "squealer", "squealed", "squeaker", "squeaked", "spreader", "spherier", "speedier", "speeches",
      "somewhen", "somerset", "someones", "soleuses", "solemner", "sneezier", "sneeshes", "sleetier", "sleepier",
      "sleekier", "sklented", "silenter", "silences", "silencer", "silenced", "sidestep", "shrewder", "shredder",
      "shredded", "shoetree", "shlepped", "shlemiel", "shielder", "shielded", "sheetfed", "sheepmen", "sheenies",
      "sheenier", "semester", "selectee", "selected", "secerned", "screwier", "screener", "screened", "screeded",
      "screamer", "screamed", "screaked", "sciences", "salesmen", "safeties", "safetied"))
    puzzle.findAllWords("evolvers")(Context(0, (3, 6, 0), Seq())) should be(List("exalters", "evolvers"))
    puzzle.findAllWords("misparse")(Context(0, (3, 6, 0), Seq())) should be(List("mispoise", "misparse"))
    puzzle.findAllWords("prefight")(Context(0, (3, 6, 0), Seq())) should be(List("prefight"))
    puzzle.findAllWords("acoustic")(Context(0, (3, 6, 0), Seq())) should be(List("aequorin", "acoustic"))
    puzzle.findAllWords("tailpipe")(Context(0, (3, 6, 0), Seq())) should be(List("trollopy", "trollops", "tailpipe"))
    puzzle.findAllWords("hornpipe")(Context(0, (3, 6, 0), Seq())) should be(List("hornpipe"))
    puzzle.findAllWords("yodelers")(Context(0, (3, 6, 0), Seq())) should be(List("yodelers"))
    puzzle.findAllWords("horsepox")(Context(0, (3, 6, 0), Seq())) should be(List("housetop", "houseboy", "horsepox"))
    puzzle.findAllWords("offstage")(Context(0, (3, 6, 0), Seq())) should be(List("outsings", "offstage"))
    puzzle.findAllWords("podocarp")(Context(0, (3, 6, 0), Seq())) should be(List("proofers", "popovers", "podomere", "podocarp"))
    puzzle.findAllWords("earwaxes")(Context(0, (3, 6, 0), Seq())) should be(List("eyewater", "entwines", "entwined", "ekpweles", "earwaxes"))
  }

}

//case class Context(rot: Int, columns: (Int, Int, Int), dictList: Seq[String])
// TODO

package net.egp.puzzle

import org.scalatest.{FlatSpec, Matchers}
import Util._

class PuzzleSpec extends FlatSpec with Matchers {
  import Util._

  val exclusionlist: Seq[String] = readLinesFromFile("exclusions.txt")
  val dictLines: Seq[String] = readLinesFromFile("words.txt")
  val rawDictList: Seq[String] = dictLines.filterNot(exclusionlist.contains)

  "The rot method" should "rotate" in {
    rot("ABC")(Context(3, 0, (0, 1, 2), Seq())) should be("abc")
    rot("ABC")(Context(3, 6, (0, 1, 2), Seq())) should be("ghi")
    rot("ABC")(Context(3, 26 - 6, (0, 1, 2), Seq())) should be("uvw")
  }

  "findAllWords for Context(0, 0, (3, 6, 0))" should "return lists" in {
    //List(saxhorns, safetied, evolvers, misparse, prefight, acoustic, tailpipe, hornpipe, yodelers, horsepox, offstage, podocarp, earwaxes)
    findAllWords("saxhorns", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List("sighting", "saxhorns"))
    findAllWords("safetied", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List(
      "sweeties", "sweepier", "sweenies", "sureties", "superset", "supermen", "superjet", "superber",
      "streusel", "stresses", "stressed", "streeled", "streeker", "streeked", "streamer", "streamed", "streaker",
      "streaked", "steeples", "steepled", "steelies", "steelier", "squegged", "squeezes", "squeezer", "squeezed",
      "squeegee", "squealer", "squealed", "squeaker", "squeaked", "spreader", "spherier", "speedier", "speeches",
      "somewhen", "somerset", "someones", "soleuses", "solemner", "sneezier", "sneeshes", "sleetier", "sleepier",
      "sleekier", "sklented", "silenter", "silences", "silencer", "silenced", "sidestep", "shrewder", "shredder",
      "shredded", "shoetree", "shlepped", "shlemiel", "shielder", "shielded", "sheetfed", "sheepmen", "sheenies",
      "sheenier", "semester", "selectee", "selected", "secerned", "screwier", "screener", "screened", "screeded",
      "screamer", "screamed", "screaked", "sciences", "salesmen", "safeties", "safetied"))
    findAllWords("evolvers", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List("exalters", "evolvers"))
    findAllWords("misparse", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List("mispoise", "misparse"))
    findAllWords("prefight", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List("prefight"))
    findAllWords("acoustic", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List("aequorin", "acoustic"))
    findAllWords("tailpipe", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List("trollopy", "trollops", "tailpipe"))
    findAllWords("hornpipe", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List("hornpipe"))
    findAllWords("yodelers", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List("yodelers"))
    findAllWords("horsepox", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List("housetop", "houseboy", "horsepox"))
    findAllWords("offstage", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List("outsings", "offstage"))
    findAllWords("podocarp", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List("proofers", "popovers", "podomere", "podocarp"))
    findAllWords("earwaxes", rawDictList)(Context(0, 0, (3, 6, 0), Seq())) should be(List("eyewater", "entwines", "entwined", "ekpweles", "earwaxes"))
  }

}

//case class Context(0, rot: Int, columns: (Int, Int, Int), dictList: Seq[String])
// TODO

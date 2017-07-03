package net.egp.puzzle

import org.scalatest.{FlatSpec, Matchers}
import Util._

class UtilSpec extends FlatSpec with Matchers {

  val exclusionlist: Seq[String] = readLinesFromFile("exclusions.txt")
  val dictLines: Seq[String] = readLinesFromFile("words.txt")
  val rawDictList: Seq[String] = dictLines.filterNot(exclusionlist.contains)

  "The rot method" should "rotate" in {
    rot("abc", 0) should be("ABC")
    rot("abc", 6) should be("GHI")
    rot("abc", 26 - 6) should be("UVW")
  }

  "findAllWords for Context(0, 0, (3, 6, 0))" should "return lists" in {
    //List(saxhorns, safetied, evolvers, misparse, prefight, acoustic, tailpipe, hornpipe, yodelers, horsepox, offstage, podocarp, earwaxes)
    findAllWords("saxhorns", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List("SIGHTING", "SAXHORNS"))
    findAllWords("safetied", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List(
      "SWEETIES", "SWEEPIER", "SWEENIES", "SURETIES", "SUPERSET", "SUPERMEN", "SUPERJET", "SUPERBER",
      "STREUSEL", "STRESSES", "STRESSED", "STREELED", "STREEKER", "STREEKED", "STREAMER", "STREAMED", "STREAKER",
      "STREAKED", "STEEPLES", "STEEPLED", "STEELIES", "STEELIER", "SQUEGGED", "SQUEEZES", "SQUEEZER", "SQUEEZED",
      "SQUEEGEE", "SQUEALER", "SQUEALED", "SQUEAKER", "SQUEAKED", "SPREADER", "SPHERIER", "SPEEDIER", "SPEECHES",
      "SOMEWHEN", "SOMERSET", "SOMEONES", "SOLEUSES", "SOLEMNER", "SNEEZIER", "SNEESHES", "SLEETIER", "SLEEPIER",
      "SLEEKIER", "SKLENTED", "SILENTER", "SILENCES", "SILENCER", "SILENCED", "SIDESTEP", "SHREWDER", "SHREDDER",
      "SHREDDED", "SHOETREE", "SHLEPPED", "SHLEMIEL", "SHIELDER", "SHIELDED", "SHEETFED", "SHEEPMEN", "SHEENIES",
      "SHEENIER", "SEMESTER", "SELECTEE", "SELECTED", "SECERNED", "SCREWIER", "SCREENER", "SCREENED", "SCREEDED",
      "SCREAMER", "SCREAMED", "SCREAKED", "SCIENCES", "SALESMEN", "SAFETIES", "SAFETIED"))
    findAllWords("evolvers", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List("EXALTERS", "EVOLVERS"))
    findAllWords("misparse", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List("MISPOISE", "MISPARSE"))
    findAllWords("prefight", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List("PREFIGHT"))
    findAllWords("acoustic", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List("AEQUORIN", "ACOUSTIC"))
    findAllWords("tailpipe", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List("TROLLOPY", "TROLLOPS", "TAILPIPE"))
    findAllWords("hornpipe", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List("HORNPIPE"))
    findAllWords("yodelers", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List("YODELERS"))
    findAllWords("horsepox", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List("HOUSETOP", "HOUSEBOY", "HORSEPOX"))
    findAllWords("offstage", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List("OUTSINGS", "OFFSTAGE"))
    findAllWords("podocarp", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List("PROOFERS", "POPOVERS", "PODOMERE", "PODOCARP"))
    findAllWords("earwaxes", rawDictList)(PuzzleContext(0, 0, (3, 6, 0), Seq())) should be(List("EYEWATER", "ENTWINES", "ENTWINED", "EKPWELES", "EARWAXES"))
  }

}

//case class Context(0, rot: Int, columns: (Int, Int, Int), dictList: Seq[String])
// TODO

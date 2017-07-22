package net.egp.puzzle

import better.files.{File => BFile}
import buildinfo.BuildInfo
import net.egp.puzzle.Util._
import org.scalactic.TypeCheckedTripleEquals._

object Themes extends App {
  assert(args.length === 1, "Expecting one argument: Theme name (without extension) found in src/main/resources/")
  val themeName = args(0)
  val myName = BuildInfo.name
  val buildInfoString = s"$myName ${BuildInfo.version} sbt ${BuildInfo.sbtVersion} scala ${BuildInfo.scalaVersion}"
  println(s"$buildInfoString\n theme-file=$themeName.names, phrase-file=$themeName.phrases")
  val attempts = new Themes(themeName).solve()
  val possibleSolutions = attempts.filter(_.isComplete) // keep ones that have 13 room names
  val solutions = possibleSolutions.filter(_.isValid) // keep ones without contention for names
  val solutionFile = BFile(s"$themeName.solutions")

  solutionFile.overwrite {
    if (solutions.nonEmpty) {
      val cr = "\n"
      solutions.mkString(cr, cr, cr)
    } else {
      "No solutions found.\n"
    }
  }
  println(s"Complete: $myName tried ${attempts.length} cases,  " +
    s"Wrote ${solutions.length} solutions to ${solutionFile.path}")
}

class Themes(theme: String) {
  lazy val requireNumberOfNames = 13
  val phraseList: Seq[Seq[Char]] = readFromResource(s"$theme.phrases").map(_.toSeq).filterNot(_.contains('0'))
  phraseList.foreach(p => assert(requireNumberOfNames === p.length))
  val nameLines: Seq[String] = readFromResource(s"$theme.names")
  println(s"read ${phraseList.length} phrases, ${nameLines.length} theme words")
  val wordsByLength: Map[Int, Seq[String]] = nameLines.groupBy(_.length)
  val maxWordLength: Int = wordsByLength.keys.max
  val minWordLength: Int = wordsByLength.keys.min
  val casesToTry: Seq[ThemeContext] =  for {
    wordLen <- minWordLength to maxWordLength
    rot <- 0 until 26
    newDict: Seq[String] = nameLines.filter(_.length >= wordLen)
    column <- 0 until wordLen
    phraseCnt <- phraseList.indices
    currentPhrase = phraseList(phraseCnt)
    ct = ThemeContext(wordLen, rot, column, currentPhrase, newDict)
  } yield ct

  def solve(): Seq[Solution] = casesToTry.map(ct => findThemeSet(requireNumberOfNames)(ct))
}


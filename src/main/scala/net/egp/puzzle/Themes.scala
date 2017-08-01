package net.egp.puzzle

import better.files.{File => BFile, _}
import buildinfo.BuildInfo
import net.egp.puzzle.Util._
import org.scalactic.TypeCheckedTripleEquals._

object Themes extends App {

  val myName = BuildInfo.name
  val buildInfoString = s"$myName ${BuildInfo.version} sbt ${BuildInfo.sbtVersion} scala ${BuildInfo.scalaVersion}"
  val resourceDir = "src"/"main"/"resources"
  val themeMatches = resourceDir.glob("*.names").toList
  println(s"$myName found ${themeMatches.length} src/main/resources/*.names")
  themeMatches.foreach{ themeFile =>
    val themeName = themeFile.nameWithoutExtension
    println(s"$buildInfoString\n theme-file=$themeName.names, phrase-file=$themeName.phrases")
    val attempts = new Themes(themeFile).solve()
    val possibleSolutions = attempts.filter(_.isComplete) // keep ones that have correct number of room names
  val solutions = possibleSolutions.filter(_.isValid) // keep ones without contention for names
  val solutionFile = BFile(s"solutions/$themeName.solutions")

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
}

class Themes(themeFile: BFile) {
  lazy val requireNumberOfNames = Room.requiredNumberOfNames
  val readNameFile = true
  val readPhraseFile = false
  val themeName = themeFile.nameWithoutExtension
  val phraseList: Seq[Seq[Char]] = readFromResource(themeFile, readPhraseFile).map(_.toSeq).filterNot(_.contains('0'))
  phraseList.foreach(p => assert(requireNumberOfNames === p.length))
  val nameLines: Seq[String] = readFromResource(themeFile, readNameFile)
  println(s"read ${phraseList.length} phrases, ${nameLines.length} theme words")
  val wordsByLength: Map[Int, Seq[String]] = nameLines.groupBy(_.length)
  val maxWordLength: Int = wordsByLength.keys.max
  val minWordLength: Int = wordsByLength.keys.min
  val casesToTry: Seq[ThemeContext] =  for {
    wordLen <- minWordLength to maxWordLength
    rot <- 0 to ('z' - 'a')
    newDict: Seq[String] = nameLines.filter(_.length >= wordLen)
    column <- 0 until wordLen
    phraseCnt <- phraseList.indices
    currentPhrase = phraseList(phraseCnt)
    ct = ThemeContext(wordLen, rot, column, currentPhrase, newDict)
  } yield ct

  def solve(): Seq[Solution] = casesToTry.map(ct => findThemeSet(requireNumberOfNames)(ct))
}

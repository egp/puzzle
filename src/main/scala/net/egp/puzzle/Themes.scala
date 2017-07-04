package net.egp.puzzle

import net.egp.puzzle.Util._
import org.scalactic.TypeCheckedTripleEquals._
import better.files.{File => BFile, _}

object Themes extends App {

  val requiredNames = 13
  assert(args.length === 2, "Expecting two args: Theme and Phrase names (withoug extension)")
  val themeFileName = args(0)
  val phraseFileName = args(1)
  println(s"Starting: Themes themeFileName=$themeFileName.txt phraseFileName=$phraseFileName.txt")
  val attempts = new Themes(themeFileName, phraseFileName ).solve()
  val solutions = attempts.filter(_.isValid)
  val solutionFile = BFile(s"$themeFileName.out")
  solutionFile.overwrite(solutions.mkString("\n", "\n", "\n"))
  println(s"Complete: Themes tried ${attempts.length} cases. " +
    s"Wrote ${solutions.length} solutions to ${solutionFile.path}")
}

class Themes(themeFile: String, phraseFile: String) {
  import Themes.requiredNames
  val phraseList: Seq[Seq[Char]] = readFromResource(s"$phraseFile.txt").map(_.toSeq).filterNot(_.contains('0'))
  phraseList.foreach(p => assert(requiredNames === p.length))
  val exclusionlist: Seq[String] = readFromResource("exclusions.txt")
  val dictLines: Seq[String] = readFromResource(s"$themeFile.txt")
  val rawDictList: Seq[String] = dictLines.filterNot(exclusionlist.contains)
  println(s"read ${phraseList.length} phrases, ${dictLines.length} theme words and ${exclusionlist.length} exclusions")
  val wordsByLength: Map[Int, Seq[String]] = rawDictList.groupBy(_.length)
  val maxWordLength: Int = wordsByLength.keys.max
  val minWordLength: Int = wordsByLength.keys.min

  def solve(): Seq[Solution] = for {
    wordLen <- minWordLength to maxWordLength
    rot <- 0 until 26
    newDict: Seq[String] = rawDictList.filter(_.length >= wordLen)
    column <- 0 until wordLen
    phraseCnt <- phraseList.indices
    currentPhrase = phraseList(phraseCnt)
    ct = ThemeContext(wordLen, rot, column, currentPhrase, newDict)
  } yield findThemeSet(requiredNames)(ct)
}


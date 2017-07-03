package net.egp.puzzle

import net.egp.puzzle.Util._
import org.scalactic.TypeCheckedTripleEquals._

object Themes extends App {
  assert(args.length === 2, "Expecing two args: Theme and Phrase filenames")
  println("Starting: Themes " + (args mkString ", "))
  val attempts = new Themes(args(0), args(1)).solve()
  val solutions = attempts.filter(_.isValid)
  println(s"Complete: Themes tried ${attempts.length} cases and found ${solutions.length} solutions")
}

class Themes(themeFile: String, phraseFile: String) {

  val requiredNames = 13
  val phraseList: Seq[Seq[Char]] = readLinesFromFile(s"$phraseFile.txt").map(_.toSeq).filterNot(_.contains('0'))
  phraseList.foreach(p => assert(13 === p.length))
  val exclusionlist: Seq[String] = readLinesFromFile("exclusions.txt")
  val dictLines: Seq[String] = readLinesFromFile(s"$themeFile.txt")
  val rawDictList: Seq[String] = dictLines.filterNot(exclusionlist.contains)
  println(s"read ${phraseList.length} phrases, ${exclusionlist.length} exclusions, and ${dictLines.length} words")
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


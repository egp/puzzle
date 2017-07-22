package net.egp.puzzle

import net.egp.puzzle.Util._
import org.scalactic.TypeCheckedTripleEquals._
import better.files.{File => BFile}

object Themes extends App {
  assert(args.length === 1, "Expecting one argument: Theme name (without extension) found in src/main/resources/")
  val themeName = args(0)
  println(s"Starting Themes: theme-file=$themeName.names, phrase-file=$themeName.phrases")
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
  println(s"Complete: Themes tried ${attempts.length} cases,  " +
    s"Wrote ${solutions.length} solutions to ${solutionFile.path}")
}

class Themes(theme: String) {
  lazy val requireNumberOfNames = 13
  val phraseList: Seq[Seq[Char]] = readFromResource(s"$theme.phrases").map(_.toSeq).filterNot(_.contains('0'))
  phraseList.foreach(p => assert(requireNumberOfNames === p.length))
  val exclusionlist: Seq[String] = readFromResource("exclusions.txt")
  val dictLines: Seq[String] = readFromResource(s"$theme.names")
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
  } yield findThemeSet(requireNumberOfNames)(ct)
}


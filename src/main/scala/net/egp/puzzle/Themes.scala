package net.egp.puzzle

import org.scalactic._
import TypeCheckedTripleEquals._
import Util._

import scala.collection.immutable

object Themes extends App {
  println("Starting: Themes " + (args mkString ", "))
  val attempts = new Themes().solve
  val solutions = attempts.filter(_.nonEmpty)
  println(s"Complete: Themes tried ${attempts.length} cases and found ${solutions.length} solutions")
}

class Themes {

  // read all the files
  val requiredNames = 13
  val phraseList: Seq[Seq[Char]] = readLinesFromFile("phrases13.txt").map(_.toSeq).filterNot(_.contains('0'))
  phraseList.foreach(p => assert(13 === p.length))
  val exclusionlist = readLinesFromFile("exclusions.txt")
  val dictLines = readLinesFromFile("FamousAmericanWomen.txt")
  val rawDictList = dictLines.filterNot(exclusionlist.contains)
  println(s"read ${phraseList.length} phrases, ${exclusionlist.length} exclusions, and ${dictLines.length} words")
  val wordsByLength: Map[Int, Seq[String]] = rawDictList.groupBy(_.length)
  //  wordsByLength.foreach{ p =>
  //    println(s" len=${p._1} count=${p._2.length}")
  //  }
  val maxWordLength = wordsByLength.map(_._1).max
  val minWordLength = wordsByLength.map(_._1).min
  //  println(s" max word length = $maxWordLength")

  def solve(): Seq[List[String]] = for {
    wordLen <- minWordLength to maxWordLength
    rot <- 0 until 26
    newDict: Seq[String] = rawDictList.filter(_.length >= wordLen)
    column <- 0 until wordLen
    phraseCnt <- phraseList.indices
    currentPhrase = phraseList(phraseCnt)
    ct = ThemeContext(wordLen, rot, column, currentPhrase, newDict)
//    _ = println(ct)
  } yield findThemeSet(requiredNames)(ct)


}


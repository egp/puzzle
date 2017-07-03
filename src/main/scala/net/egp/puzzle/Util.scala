package net.egp.puzzle

import scala.io.Source
import org.scalactic._
import TypeCheckedTripleEquals._


object Util {

  /**
    * reads lines from a file
    *
    * @param fn - filename of file to load
    * @return - a list of lines from the specified file
    */
  def readLinesFromFile(fn: String): Seq[String] = {
    val fileSource = Source.fromResource(fn)
    val linesFromFile = try {
      val rawLines = fileSource.getLines.toSeq
      println(s"${fn}\t\tlines=${rawLines.length}")
      rawLines.map(_.trim.toUpperCase).filter(_.nonEmpty)
    } finally {
      fileSource.close()
    }
    linesFromFile
  }

  /**
    * rotates a word, using caesar cipher method
    *
    * @param s     - the word to be rotated
    * @param xform - contains the required rot value
    * @return - the rotated word
    */
  def rot(s: String, rot: Int): String = {
    assert(0 <= rot && rot < 26)
    val ls = s.toUpperCase
    ls.map { c => (c.toInt - 'A'.toInt + rot) % 26 }
      .map(i => (i + 'A').toChar).mkString
  }


  /**
    * selects specified columns from a word
    *
    * @param wd  - the word from which columns will be selected
    * @param cxt - contains the list of columns to select
    * @return - just the selected columns in the order specified
    */
  def cols(wd: String)(implicit cxt: PuzzleContext): String =
    s"${wd(cxt.columns._1)}${wd(cxt.columns._2)}${wd(cxt.columns._3)}"

  /**
    * finds all similar words that have the same rot and columns
    *
    * @param matchWord - word found by solver
    * @param cxt       - specifies the rot and columns
    * @return - a list of similar words
    */
  def findAllWords(matchWord: String, rawDictList: Seq[String])(implicit cxt: PuzzleContext): List[String] = {

    @annotation.tailrec
    def allWordsLoop(remainingDict: List[String], acc: List[String])(implicit cxt: PuzzleContext): List[String] =
      remainingDict match {
        case Nil => acc
        case dictWord :: tl =>
          val nextAcc = if (cols(matchWord).equalsIgnoreCase(cols(rot(dictWord, cxt.rot)))) dictWord :: acc else acc
          allWordsLoop(tl, nextAcc)
      }

    allWordsLoop(rawDictList.filter(_.length === matchWord.length).toList, Nil)
  }

  /**
    * finds a word in the dictionary that matches the specified rot and columns
    *
    * @param matchWord - word to be matched
    * @param cxt       - specifies the rot and columns
    * @return - a dictionary word if successful
    */
  def findWord(matchWord: String)(implicit cxt: PuzzleContext): Option[String] =
    cxt.dictList.find(wd => matchWord.take(3).equalsIgnoreCase(cols(rot(wd, cxt.rot))))

  //    cxt.dictList.map { dictWord =>
  //      if (matchWord.substring(0, 3).equalsIgnoreCase(cols(rot(dictWord)))) Some(dictWord) else None
  //    }.find(_.isDefined).flatten

  /**
    * Find a set of words such that the goal phrase is embedded in the columns
    *
    * @param acc - results so far
    * @param xg  - goal strings (broken by room name)
    * @param cxt - current context
    * @return - list of words
    */
  @annotation.tailrec
  final def findWordSet(acc: List[String], xg: List[String])(implicit cxt: PuzzleContext): List[String] =
  xg match {
    case Nil => acc.reverse
    case hd :: tl if findWord(hd).nonEmpty => findWordSet(findWord(hd).get :: acc, tl)
    case _ => Nil
  }


  def findThemeSet(wordsToFind: Int)(implicit tc: ThemeContext): List[String] = {
    //    @annotation.tailrec
    def findThemeSetLoop(acc: List[String], numWordsFound: Int): List[String] = {
      numWordsFound match {
        case _ if numWordsFound >= wordsToFind => acc.reverse
        case _ if findThemeWord(numWordsFound).nonEmpty =>
          val found = findThemeWord(numWordsFound).getOrElse(throw new Exception("???"))
          findThemeSetLoop(found :: acc, numWordsFound + 1)
        case _ => Nil
      }
    }

    val retVal = findThemeSetLoop(Nil, 0)
    if (retVal.nonEmpty) {
      assert(retVal.length === wordsToFind)
      println(tc.toString, retVal)
    }
    retVal
  }

  def findThemeWord(rowNum: Int)(implicit tc: ThemeContext): Option[String] =
    tc.dictList.find(w => rot(w, tc.rot)(tc.column) === tc.currentPhase(rowNum))

}

// miscellaneous case classes

case class PuzzleContext(wordLen: Int, rot: Int, columns: (Int, Int, Int), dictList: Seq[String]) {
  val (c1, c2, c3) = columns

  override def toString(): String = s"c(l=$wordLen rot=$rot,col=$c1,$c2,$c3)"
}

case class ThemeContext(wordLen: Int,
                        rot: Int,
                        column: Int,
                        currentPhase: Seq[Char],
                        dictList: Seq[String]) {
  override def toString: ErrorMessage = s"s(l=$wordLen r=$rot c=$column, p=$currentPhase)"
}

case class Solution(cxt: PuzzleContext, solution: Seq[RoomChoices]) {
  override def toString: String = cxt.toString() + "\n" +
    solution.zipWithIndex.map(rci => s"${rci._2 + 1}. ${rci._1.toString()} \n")
}

case class RoomChoices(names: List[String]) {
  override def toString: String = names mkString("[", ",", "]")
}


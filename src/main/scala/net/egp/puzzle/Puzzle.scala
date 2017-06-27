package net.egp.puzzle

import scala.io.Source

object Puzzle extends App {
  val puzzle = new Puzzle()
  val solution = puzzle.solve()
  println(solution)
}

class Puzzle() {

  /**
    * Puzzle solver
    *
    * assumes three vertical columns contain the goal phrase.
    * Goal phrase length must be multiple of three
    */
  val requiredColumns = 3
  // read all the files
  val phraseList: Seq[Seq[Char]] = readLinesFromFile("phrases.txt").map(_.toSeq)
  phraseList.foreach(p => assert(0 == p.length % requiredColumns))
  val exclusionlist: Seq[String] = readLinesFromFile("exclusions.txt")
  val dictLines: Seq[String] = readLinesFromFile("words.txt")
  val rawDictList: Seq[String] = dictLines.filterNot(exclusionlist.contains)
  println(s"read ${phraseList.length} phrases, ${exclusionlist.length} exclusions, and ${dictLines.length} words")

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
      rawLines.map(_.trim.toLowerCase).filter(_.nonEmpty)
    } finally {
      fileSource.close()
    }
    linesFromFile
  }

  /**
    * rotates a word, using caesar cipher method
    *
    * @param s - the word to be rotated
    * @param xform - contains the required rot value
    * @return - the rotated word
    */
  def rot(s: String)(implicit xform: Context): String = {
    assert(0 <= xform.rot && xform.rot < 26)
    val ls = s.toLowerCase
    ls.map { c => (c.toInt - 'a'.toInt + xform.rot) % 26 }
      .map(i => (i + 'a').toChar).mkString
  }

  /**
    * selects specified columns from a word
    *
    * @param wd - the word from which columns will be selected
    * @param cxt - contains the list of columns to select
    * @return - just the selected columns in the order specified
    */
  def cols(wd: String)(implicit cxt: Context): String =
    s"${wd(cxt.columns._1)}${wd(cxt.columns._2)}${wd(cxt.columns._3)}"

  /**
    * finds all similar words that have the same rot and columns
    *
    * @param matchWord - word found by solver
    * @param cxt - specifies the rot and columns
    * @return - a list of similar words
    */
  def findAllWords(matchWord: String)(implicit cxt: Context): List[String] = {

    @annotation.tailrec
    def allWordsLoop(remainingDict: List[String], acc: List[String])(implicit cxt: Context): List[String] =
      remainingDict match {
        case Nil => acc
        case dictWord :: tl =>
          val nextAcc = if (cols(matchWord).equalsIgnoreCase(cols(rot(dictWord)))) dictWord :: acc else acc
          allWordsLoop(tl, nextAcc)
      }

    allWordsLoop(rawDictList.filter(_.length == matchWord.length).toList, Nil)
  }

  /**
    * finds a word in the dictionary that matches the specified rot and columns
    *
    * @param matchWord - word to be matched
    * @param cxt - specifies the rot and columns
    * @return - a dictionary word if successful
    */
  def findWord(matchWord: String)(implicit cxt: Context): Option[String] =
    cxt.dictList.map { dictWord =>
      if (matchWord.substring(0, 3).equalsIgnoreCase(cols(rot(dictWord)))) Some(dictWord) else None
    }.find(_.isDefined).flatten

  @annotation.tailrec
  final def findWordSet(acc: List[String], xg: List[String])(implicit cxt: Context): List[String] =
    xg match {
      case Nil => acc.reverse
      case hd :: tl if findWord(hd).nonEmpty => findWordSet(findWord(hd).get :: acc, tl)
      case _ => Nil
    }

  def printResults(result: List[String])(implicit cxt: Context): Unit = {
    println(cxt.toString())
    result.foreach{ word =>
      val wordList = findAllWords(word)
      println(wordList)
    }
  }

  /**
    * this method enumerates all the possible cases of:
    *   - word length
    *   - caesar cipher rotation
    *   - various columns for goal phrase
    *
    * @return all the results for all cases
    */
  def solve(): Seq[List[String]] = for {
    curLen <- 7 to 14
    currentRot <- 0 until 26
    currentSet: (Int, Int, Int) <- for {
      i <- 0 until curLen
      j <- 0 until curLen if i != j
      k <- 0 until curLen if k != i && k != j
    } yield (i, j, k)

    newDict: Seq[String] = rawDictList.filter(_.length == curLen)
    phraseCnt <- phraseList.indices
    currentPhrase = phraseList(phraseCnt)
    nameCount = currentPhrase.length / 3

    goals = (0 until nameCount).map { i =>
      s"${currentPhrase(i)}${currentPhrase(i + nameCount)}${currentPhrase(i + 2 * nameCount)}"
    }.toList

    ct = Context(currentRot, currentSet, newDict)
    result = findWordSet(Nil, goals)(ct)
    _ = if (result.nonEmpty) printResults(result)(ct)
  } yield result
}

case class Context(rot: Int, columns: (Int, Int, Int), dictList: Seq[String]) {
  val (c1, c2, c3) = columns

  override def toString(): String = s"c(rot=$rot,col=$c1,$c2,$c3)"
}

// TODO: generate many more phrases
//eof

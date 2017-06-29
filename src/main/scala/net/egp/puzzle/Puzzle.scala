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
  val requiredNames = 13
  // read all the files
  val phraseList: Seq[Seq[Char]] = readLinesFromFile("phrases.txt").map(_.toSeq).filterNot(_.contains('0'))
  phraseList.foreach(p => assert((requiredColumns * requiredNames) == p.length))
  val exclusionlist: Seq[String] = readLinesFromFile("exclusions.txt")
  val dictLines: Seq[String] = readLinesFromFile("words.txt")
  val rawDictList: Seq[String] = dictLines.filterNot(exclusionlist.contains)
  println(s"read ${phraseList.length} phrases, ${exclusionlist.length} exclusions, and ${dictLines.length} words")
  val wordsByLength: Map[Int, Seq[String]] = rawDictList.groupBy(_.length)

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
    cxt.dictList.find(wd => matchWord.take(3).equalsIgnoreCase(cols(rot(wd))))
//    cxt.dictList.map { dictWord =>
//      if (matchWord.substring(0, 3).equalsIgnoreCase(cols(rot(dictWord)))) Some(dictWord) else None
//    }.find(_.isDefined).flatten

  /**
    * Find a set of words such that the goal phrase is embedded in the columns
    *
    * @param acc - results so far
    * @param xg - goal strings (broken by room name)
    * @param cxt - current context
    * @return - list of words
    */
  @annotation.tailrec
  final def findWordSet(acc: List[String], xg: List[String])(implicit cxt: Context): List[String] =
    xg match {
      case Nil => acc.reverse
      case hd :: tl if findWord(hd).nonEmpty => findWordSet(findWord(hd).get :: acc, tl)
      case _ => Nil
    }

  /**
    * Format the results, showing the context
    *
    * @param result - the resulting list of first-found words
    * @param cxt - the context
    */
  def printResults(result: List[String])(implicit cxt: Context): Unit = {
    println(cxt.toString())
    result.foreach{ word =>
      val wordList = findAllWords(word) // show similar words
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
    curLen <- 6 to 15 // word length
    currentSet: (Int, Int, Int) <- for {
      i <- 0 until curLen
      j <- 0 until curLen if i != j
      k <- 0 until curLen if k != i && k != j
    } yield (i, j, k)
    currentRot <- 0 until 26
    newDict: Seq[String] = wordsByLength(curLen)
    phraseCnt <- phraseList.indices
    currentPhrase = phraseList(phraseCnt)
    _ = println(s"****** l$curLen r$currentRot c$currentSet $currentPhrase ******")
    nameCount = currentPhrase.length / 3
    goals = (0 until nameCount).map { i =>
      s"${currentPhrase(i)}${currentPhrase(i + nameCount)}${currentPhrase(i + 2 * nameCount)}"
    }.toList
    ct = Context(curLen, currentRot, currentSet, newDict)
    result = findWordSet(Nil, goals)(ct)
    _ = if (result.nonEmpty) printResults(result)(ct)
  } yield result
}

case class Context(wordLen: Int, rot: Int, columns: (Int, Int, Int), dictList: Seq[String]) {
  val (c1, c2, c3) = columns

  override def toString(): String = s"c(l=$wordLen rot=$rot,col=$c1,$c2,$c3)"
}

// TODO: generate many more phrases (length must be multiple of 13, i.e. 26, 39, 52)
//eof

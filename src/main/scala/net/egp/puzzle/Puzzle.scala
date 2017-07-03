package net.egp.puzzle

import net.egp.puzzle.Util._
import org.scalactic.TypeCheckedTripleEquals._
import org.scalactic._

//object Puzzle extends App {
//  val puzzle = new Puzzle()
//  val solution = puzzle.solve()
//  println(solution)
//}

class Puzzle {

  /**
    * Puzzle solver
    *
    * assumes three vertical columns contain the goal phrase.
    * Goal phrase length must be multiple of three
    */
  val requiredColumns = 3
  val requiredNames = 13
  // read all the files
  val phraseList: Seq[Seq[Char]] = readLinesFromFile("phrases39.txt").map(_.toSeq).filterNot(_.contains('0'))
  phraseList.foreach(p => assert((requiredColumns * requiredNames) === p.length))
  val exclusionlist: Seq[String] = readLinesFromFile("exclusions.txt")
  val dictLines: Seq[String] = readLinesFromFile("words.txt")
  val rawDictList: Seq[String] = dictLines.filterNot(exclusionlist.contains)
  println(s"read ${phraseList.length} phrases, ${exclusionlist.length} exclusions, and ${dictLines.length} words")
  val wordsByLength: Map[Int, Seq[String]] = rawDictList.groupBy(_.length)

  /**
    * Format the results, showing the context
    *
    * @param result - the resulting list of first-found words
    * @param cxt - the context
    */
  def printResults(result: List[String])(implicit cxt: PuzzleContext): Unit = {
    println(cxt.toString())
    result.foreach{ word =>
      val wordList = findAllWords(word, rawDictList) // show similar words
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
    ct = PuzzleContext(curLen, currentRot, currentSet, newDict)
    result = findWordSet(Nil, goals)(ct)
    _ = if (result.nonEmpty) printResults(result)(ct)
  } yield result
}


// TODO: generate many more phrases (length must be multiple of 13, i.e. 26, 39, 52)
//eof

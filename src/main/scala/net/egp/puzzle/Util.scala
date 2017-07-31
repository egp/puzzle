package net.egp.puzzle
import better.files.{File => BFile, _}
import org.scalactic.TypeCheckedTripleEquals._
import scala.io.{BufferedSource, Source}

object Util {

  /**
    * Reads lines from a file in the resource directory
    * If reading phrases, also reads Generic.phrases
    *
    * @param file - filename
    * @return - complete file as a sequence of lines
    */
  def readFromResource(file: BFile, names: Boolean): Seq[String] = {
    if (names) {
      file.lines.toSeq
    } else {
      ("src" / "main" / "resources" / (file.nameWithoutExtension + ".phrases")).lines.toSeq ++
        ("src" / "main" / "resources" / "Generic.phrases").lines.toSeq
    }.map(_.trim.toUpperCase).filter(_.nonEmpty)

  /**
    * reads lines from a file
    *
    * @param fn - filename of file to load
    * @return - a list of lines from the specified file
    */
  def readLinesFromFile(fn: String): Seq[String] = {
    val fileSource: BufferedSource = Source.fromResource(fn)
    val linesFromFile = try {
      val rawLines = fileSource.getLines.toSeq
      rawLines.map(_.trim.toUpperCase).filter(_.nonEmpty)
    } catch {
      case e: Exception =>
        println(s"Failed to read from $fn $e")
        Seq()
    }
    finally {
      if (null != fileSource) fileSource.close()
    }
    linesFromFile
  }

  /**
    * rotates a word, using caesar cipher method
    *
    * @param s   - the word to be rotated
    * @param rot - contains the required rot value
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


  def findThemeSet(wordsToFind: Int)(implicit tc: ThemeContext): Solution = {
    //    @annotation.tailrec
    val unsolved = Solution(tc, List())

    def findThemeSetLoop(acc: Solution, numWordsFound: Int): Solution = {
      numWordsFound match {
        case _ if numWordsFound >= wordsToFind =>
          Solution(tc, acc.solution.reverse)
        case _ if findThemeWord(numWordsFound).names.nonEmpty => {
          val found = findThemeWord(numWordsFound)
          findThemeSetLoop(Solution(tc, found :: acc.solution), numWordsFound + 1)
        }
        case _ =>
          unsolved
      }
    }

    findThemeSetLoop(unsolved, 0)
  }

  def findThemeWord(rowNum: Int)(implicit tc: ThemeContext): RoomChoices =
    RoomChoices(tc.dictList.filter(w => rot(w, tc.rot)(tc.column) === tc.currentPhase(rowNum)))

}

// miscellaneous case classes

case class PuzzleContext(wordLen: Int, rot: Int, columns: (Int, Int, Int), dictList: Seq[String]) {
  val (c1, c2, c3) = columns

  override def toString(): String = s"c(l=$wordLen rot=$rot, col=$c1,$c2,$c3)"
}

case class ThemeContext(wordLen: Int,
                        rot: Int,
                        column: Int,
                        currentPhase: Seq[Char],
                        dictList: Seq[String]) {
  override def toString: String = s"«wordLen=$wordLen rot=$rot column=$column, currentPhase=$currentPhase»"
}

trait PossibleSolution {
  def isValid: Boolean

  override def toString: String
}

case class Solution(cxt: ThemeContext, solution: List[RoomChoices]) extends PossibleSolution {

  def isComplete: Boolean = solution.length === Room.requiredNumberOfNames

  def isValid: Boolean = {
    val stats: Map[SolutionStats, List[RoomChoices]] = solution.groupBy(SolutionStats(_))
    stats.keys.forall { key => stats(key).length <= key.length }
  }

  case class SolutionStats(names: Seq[String], length: Int)

  object SolutionStats {
    def apply(rc: RoomChoices): SolutionStats = SolutionStats(rc.names, rc.names.length)
  }

  override def toString: String = cxt.toString() + "\n" +
    solution.zipWithIndex.map(roomChoiceIndex => {
      val (roomChoice, roomIndex) = roomChoiceIndex
      s"${roomIndex + 1}. ${roomChoice.toString()} \n"
    }).mkString
}

case object NoSolution extends PossibleSolution {
  val isValid = false
  override val toString: String = "NoSolution"
}

case class RoomChoices(names: Seq[String]) {
  override def toString: String = names mkString("[", ", ", "]")
}

object Room {
  val requiredNumberOfNames = 12
}

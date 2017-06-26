package net.egp.puzzle

import scala.io.Source

object Puzzle extends App {
  val puzzle = new Puzzle()
  val solution = puzzle.solve()
  println(solution)
}

class Puzzle() {

  lazy val phraseList: Seq[Seq[Char]] = readLinesFromFile("phrases.txt").map(_.toSeq)
  phraseList.foreach(p => assert(0 == p.length % 3))
  lazy val exclusionlist: Seq[String] = readLinesFromFile("exclusions.txt")
  lazy val dictLines: Seq[String] = readLinesFromFile("words.txt")
  lazy val rawDictList = dictLines.filterNot(exclusionlist.contains)
  println(s"read ${phraseList.length} phrases, ${exclusionlist.length} exclusions and ${dictLines.length} words")


  def rot(s: String)(implicit xform: Context): String = {
    assert(0 <= xform.rot && xform.rot < 26)
    val ls = s.toLowerCase
    ls.map { c => (c.toInt - 'a'.toInt + xform.rot) % 26 }
      .map(i => (i + 'a').toChar).mkString
  }


  def readLinesFromFile(fn: String): Seq[String] = {
    val resourceDir = "/Users/edward.prentice/egp/puzzle/src/main/resources/"
    val fullPathName = s"$resourceDir$fn"
    val fileSource = Source.fromFile(fullPathName)
    val linesFromFile = try {
      val rawLines = fileSource.getLines.toSeq
      println(s"rawLines.length=${rawLines.length}")
        rawLines.map(_.trim.toLowerCase).filter(_.nonEmpty)
    } finally {
      fileSource.close()
    }
    println(s"read ${linesFromFile.length} lines from $fn")
    linesFromFile
  }


  def findAllWords(matchWord: String, goal: String)(implicit cxt: Context): List[String] = {

    def allWordsLoop(remainingDict: Seq[String], acc: List[String])(implicit cxt: Context): List[String] = {
      remainingDict match {
        case Nil => acc
        case dictWord :: tl =>
          val wd = rot(dictWord)
          val threeDict = s"${wd(cxt.columns._1)}${wd(cxt.columns._2)}${wd(cxt.columns._3)}"
          if (goal.equalsIgnoreCase(threeDict)) {
            allWordsLoop(tl, dictWord :: acc)
          } else {
            allWordsLoop(tl, acc)
          }
      }
    }

    allWordsLoop(rawDictList.filter(_.length == matchWord.length), Nil)
  }


  def findWord(matchWord: String)(implicit cxt: Context): Option[String] = {
    cxt.dictList.map { dictWord =>
      val wd = rot(dictWord)
      val threeDict = s"${wd(cxt.columns._1)}${wd(cxt.columns._2)}${wd(cxt.columns._3)}"
      if (matchWord.substring(0, 3).equalsIgnoreCase(threeDict)) {
        //        println(s"found $dictWord, wd=$wd, ${matchWord.substring(0,3)} = $threeDict")
        Some(dictWord)
      } else {
        None
      }
    }.find(_.isDefined).flatten
  }

  @annotation.tailrec
  final def findWordSet(acc: List[String], xg: List[String])(implicit cxt: Context): List[String] = xg match {
    case Nil =>
      //      println(acc.reverse)
      acc.reverse
    case hd :: tl if findWord(hd).nonEmpty =>
      //      if (acc.length > 10) println(s"rot=${cxt.rot} ${findWord(hd).get :: acc}")
      findWordSet(findWord(hd).get :: acc, tl)
    case _ => Nil
  }

  def solve(): Seq[List[String]] =  for {
      curentWordLength <- 3 to 28
      currentRot <- 0 until 26
      currentSet: (Int, Int, Int) <- for {
        i <- 0 until curentWordLength
        j <- 0 until curentWordLength if i != j
        k <- 0 until curentWordLength if k != i && k != j
      } yield (i, j, k)

      newDict: Seq[String] = rawDictList.filter(_.length == curentWordLength)
      phraseCnt <- phraseList.indices
      currentPhrase = phraseList(phraseCnt)
      nameCount = currentPhrase.length / 3

      goals = (0 until nameCount).map { i =>
        s"${currentPhrase(i)}${currentPhrase(i + nameCount)}${currentPhrase(i + 2 * nameCount)}"
      }.toList

      ct = Context(currentRot, currentSet, newDict)
//      _ = println(s"len=$curentWordLength rot=${ct.rot} col=${ct.columns} phrase=$currentPhrase")
      result = findWordSet(Nil, goals)(ct)
      _ = if (result.nonEmpty) println(s"xform=$currentRot, result=$result")
  } yield result
}

case class Context(rot: Int, columns: (Int, Int, Int), dictList: Seq[String])

// TODO
// * generate many more phrases
//eof

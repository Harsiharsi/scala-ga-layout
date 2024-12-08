package layout.analyzing

import scala.io.{Source => S}
import java.io.PrintWriter

import layout.TypeAlias._
import layout.layouts.{LayoutJa => LT}
import layout.evaluations.{Evaluator => E}
import layout.evaluations.types._
import layout.analyzing.LayoutCharToKeyMaps._


object LayoutAnalyzer {

  case class LayoutData(
    val fitness: Double,
    val chunkFreqs: Map[HandSpecifiedChunk, Int],
    val actionFreqs: Map[List[HandSpecifiedChunk], Int],
    val totalChunkFreqs: Int,
    val totalActionFreqs: Int,
    val keysInChunkFreqs: Map[Int, Int],
    val keysInActionFreqs: Map[Int, Int],
    val listOfNgramAndAction: List[(String, List[List[HandSpecifiedChunk]], Int, Double)]
  )

  def getLayoutData(charToKey: Map[String, String]): LayoutData = {
    def rec(
      charToKey: Map[String, String],
      ngrams: List[(String, Int)],
      fitness: Double,
      chunkFreqs: Map[HandSpecifiedChunk, Int],
      actionFreqs: Map[List[HandSpecifiedChunk], Int],
      totalChunkFreqs: Int,
      totalActionFreqs: Int,
      keysInChunkFreqs: Map[Int, Int],
      keysInActionFreqs: Map[Int, Int],
      listOfNgramAndAction: List[(String, List[List[HandSpecifiedChunk]], Int, Double)]
    ): LayoutData = ngrams match {
      case Nil => LayoutData(fitness, chunkFreqs, actionFreqs, totalChunkFreqs, totalActionFreqs,
                             keysInChunkFreqs, keysInActionFreqs, listOfNgramAndAction)
      case (ngram, frequency)::xs =>
        val (keyStrokes, inputChars) = LT.assignableCharsToPhysicalKeys(ngram, charToKey)

        val chunks = E.partitionedKeysToChunks(E.partitionKeysByHand(keyStrokes))
        val (actions, score) = E.chunksToListOfActionChunks(chunks, true)

        val newFitness = fitness + score * frequency
        val newChunkFreqs = chunkFreqs ++ chunks.map(x => x -> (chunkFreqs.get(x).getOrElse(0) + frequency)).toMap
        val newActionFreqs = actionFreqs ++ actions.map(xs => xs -> (actionFreqs.get(xs).getOrElse(0) + frequency)).toMap
        val newTotalChunkFreqs = totalChunkFreqs + chunks.length * frequency
        val newTotalActionFreqs = totalActionFreqs + actions.length * frequency
        val newKeysInChunkFreqs = keysInChunkFreqs ++ chunks.map { x =>
          val keyLen = x.keys.length
          keyLen -> (keysInChunkFreqs.get(keyLen).getOrElse(0) + frequency)
        } .toMap
        val newKeysInActionFreqs = keysInActionFreqs ++ actions.map { xs =>
          val keyLen = xs.map(_.keys).mkString.length
          keyLen -> (keysInActionFreqs.get(keyLen).getOrElse(0) + frequency)
        }
        val newListOfNgramAndAction = if (ngram.length == inputChars.length)
          (ngram, actions, frequency, score * frequency)::listOfNgramAndAction
        else
          listOfNgramAndAction

        rec(charToKey, xs, newFitness, newChunkFreqs, newActionFreqs,
            newTotalChunkFreqs, newTotalActionFreqs, newKeysInChunkFreqs,
            newKeysInActionFreqs, newListOfNgramAndAction)
    }
    rec(charToKey, LT.ngrams.toList, 0.0, Map(), Map(), 0, 0, Map(), Map(), Nil)
  }

  def writeAllLayouts(): Unit = {
    List(
      ("月配列2-263", "tsuki2_263", tsuki2_263),
      ("月見草配列V2", "tsukimisouV2", tsukimisouV2),
      ("幸花配列", "yukika", yukika),
      ("中指シフト月光20210622", "gekkou20210622", gekkou20210622),
      ("月配列U9", "tsukiU9", tsukiU9),
      ("ぶな配列2.0", "buna2_0", buna2_0),
      ("ミズナラ配列1.0", "mizunara1_0", mizunara1_0),
      ("コンポジション#1", "compositionNo1", compositionNo1),
      ("コンポジション#2", "compositionNo2", compositionNo2),
      ("コンポジション#3", "compositionNo3", compositionNo3)
    ).foreach { case (layoutName, fileName, charToKey) => writeFiles(layoutName, fileName, getLayoutData(charToKey)) }
  }

  def writeFiles(layoutName: String, fileName: String, layoutData: LayoutData): Unit = {
    val basicDataFile = new PrintWriter("./src/main/resources/layout_analyzed/" + fileName + "_basic_data.txt", "UTF-8")
    basicDataFile.println(layoutName)
    basicDataFile.println()
    basicDataFile.println("適応度:\t" + layoutData.fitness.toString)
    basicDataFile.println("総チャンク数:\t" + layoutData.totalChunkFreqs.toString)
    basicDataFile.println()
    basicDataFile.println("チャンクに含まれる打鍵数ごとの頻度")
    layoutData.keysInChunkFreqs.toList.sorted.foreach { case (strokeNumber, frequency) =>
      basicDataFile.println(s"$strokeNumber" + s"打鍵チャンク:\t$frequency")
    }
    basicDataFile.println()
    basicDataFile.println("チャンク頻度")
    layoutData.chunkFreqs.toList.sortBy(_._2).foreach { case (chunk, frequency) =>
      basicDataFile.println(s"$chunk\t$frequency")
    }
    basicDataFile.close()

    val actionDataFile = new PrintWriter("./src/main/resources/layout_analyzed/" + fileName + "_action_data.txt", "UTF-8")
    actionDataFile.println(layoutName)
    actionDataFile.println()
    actionDataFile.println("総アクション数:\t" + layoutData.totalActionFreqs.toString)
    actionDataFile.println()
    actionDataFile.println("チャンクに含まれる打鍵数ごとの頻度")
    layoutData.keysInActionFreqs.toList.sorted.foreach { case (strokeNumber, frequency) =>
      actionDataFile.println(s"$strokeNumber" + s"打鍵アクション:\t$frequency")
    }
    actionDataFile.println()
    actionDataFile.println("アクション頻度")
    layoutData.actionFreqs.toList.sortBy(_._2).foreach { case (action, frequency) =>
      val actionString = action.mkString(" ")
      actionDataFile.println(s"$actionString\t$frequency")
    }
    actionDataFile.close()

    val ngramToactionFile = new PrintWriter("./src/main/resources/layout_analyzed/" + fileName + "_ngram_to_actions.txt", "UTF-8")
    ngramToactionFile.println(layoutName)
    ngramToactionFile.println()
    ngramToactionFile.println("ngram\tアクション列\t出現頻度\t(出現頻度順)")
    layoutData.listOfNgramAndAction.toList.sortBy { case (_, _, frequency, _) => -frequency }
      .take(5000).foreach {
        case (ngram, actions, frequency, _) =>
          val actionsString = actions.map(x => "(" + x.mkString(" ") + ")").mkString(" ")
          ngramToactionFile.println(s"$ngram\t$actionsString\t$frequency")
      }
    ngramToactionFile.close()
  }
}

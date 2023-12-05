package layout

import scala.collection.{mutable => mut}
import scala.util.Try
import scala.annotation.tailrec

import layout.TypeAlias._


class Evaluator(
  val leftKeys: List[String],
  val rightKeys: List[String],
  val typingPatterns: Map[String, Double],
  val chunkPatterns: Map[String, Double],
  val nonChunkPatterns: Map[String, Double]
) {
  import layout.Evaluator._


  def apply(typingString: String): Double = evaluate(typingString)

  def evaluate(typingString: String): Double = {
    val chunks: List[Chunk] = typingStringToChunks(typingString)
    val actions: List[HandAction] = chunksToHandActions(chunks)
    val interactions: List[NonChunk] = actionsToInteractions(actions)

    val actionsScore: Double = (for (a <- actions) yield {
      val leftAction: HandAction = a.filter(x => leftKeys.contains(x.head.toString))
      val rightAction: HandAction = a.filter(x => !leftAction.contains(x))

      (for (oneHand <- List(leftAction, rightAction)) yield {
        memorizedOneHandActions.get(oneHand) match {
          case Some(x) =>
            memorizedOneHandActions += oneHand -> x

            x
          case None =>
            val score = evaluateHandAction(oneHand)
            memorizedOneHandActions += oneHand -> score

            score
        }
      }).sum
    }).toList.sum
    val interactionsScore = interactions.map(x => typingPatterns.get(x)).flatten.sum * 1.4

    actionsScore + interactionsScore
  }

  def typingStringToChunks(s: String): List[Chunk] = {
    @tailrec
    def f(s: String, chunks: List[Chunk]): List[Chunk] = s match {
      case "" => chunks.reverse
      case s => 
        val candidates: List[(String, String)] = (for (i <- (1 to 4).reverse) yield s.splitAt(i)).toList
        val filtered = candidates.filter(t => chunkPatterns.isDefinedAt(t._1))
        val (chosenChunk, restString): (Chunk, String) = filtered.head
        val newChunks = chosenChunk +: chunks

        f(restString, newChunks)
    }

    f(s, List[Chunk]())
  }

  def chunksToHandActions(chunks: List[Chunk]): List[HandAction] = {
    @tailrec
    def f(chunks: List[Chunk], a: HandAction, actions: List[HandAction]): List[HandAction] = chunks match {
      case Nil => (a.reverse +: actions).reverse
      case ch::restChunks =>
        val worstInterchunk: Interchunk = {
          val leftOrRight = if (leftKeys.contains(ch.head.toString)) true else false
          val sameHandChunks: List[Chunk] = a.filter(s => leftKeys.contains(s.head.toString) == leftOrRight)
          val totalTypingStringOfHandAction: String = sameHandChunks.mkString
          val interchunks = makeInterchunks(ch, totalTypingStringOfHandAction)

          Try(interchunks.maxBy(typingPatterns(_))).getOrElse("")
        }
        val (newA, newActions) = nonChunkPatterns.get(worstInterchunk) match {
          case Some(_) => (List(ch), a.reverse +: actions)
          case None => (ch +: a, actions)
        }

        f(restChunks, newA, newActions)
    }

    f(chunks, List[String](), List[HandAction]())
  }

  def actionsToInteractions(actions: List[HandAction]): List[NonChunk] = {
    val actionPairs: List[(HandAction, HandAction)] = actions.dropRight(1).zip(actions.drop(1))

    actionPairs.map{case (formerAction, latterAction) =>
      val formerLeftAction: String =
        formerAction.filter(x => leftKeys.contains(x.head.toString)).mkString
      val latterLeftAction: String =
        latterAction.filter(x => leftKeys.contains(x.head.toString)).mkString
      val formerRightAction: String =
        formerAction.filter(x => rightKeys.contains(x.head.toString)).mkString
      val latterRightAction: String =
        latterAction.filter(x => rightKeys.contains(x.head.toString)).mkString

      val leftPairAndRightPair = List(
        formerLeftAction -> latterLeftAction,
        formerRightAction -> latterRightAction
      )

     val leftAndRightWorstInterchunks = leftPairAndRightPair.map{case (former, latter) => makeInterchunks(former, latter) match {
       case Nil => None
       case xs => Option(xs.maxBy(typingPatterns(_)))
     }}.flatten

     Try(leftAndRightWorstInterchunks.maxBy(typingPatterns(_))).getOrElse("")
    }.filter(_ != "")
  }

  def evaluateHandAction(a: HandAction): Double = {
    val pairsOfFormerChunksAndChunk: List[(String, Chunk)] = {
      val l: List[List[Chunk]] = (for (i <- 0 to a.length - 1) yield {
        a.dropRight(i)
      }).toList

      l.map(x => (x.init.mkString, x.last))
    }

    (for ((formerChunks, ch) <- pairsOfFormerChunksAndChunk) yield {
      val chunkScore = typingPatterns(ch)
      val interchunks = makeInterchunks(formerChunks, ch)
      val worstInterchunkScore = Try(interchunks.map(chunkPatterns(_)).max)
      val penalty = chunkPatterns.get(formerChunks + ch) match {
        case Some(_) => 1.1
        case None => 1.3
      }

      chunkScore + worstInterchunkScore.getOrElse(0.0) * penalty
    }).sum
  }

  def makeInterchunks(chunk1: Chunk, chunk2: Chunk): List[Interchunk] = {
    (for (c1 <- chunk1; c2 <- chunk2) yield c1.toString + c2.toString).toList
  }

}

object Evaluator {
  private val memorizedOneHandActions = mut.Map[HandAction, Double]()

  def apply(
    leftKeys: List[String],
    rightKeys: List[String],
    typingPatterns: Map[String, Double],
    chunkPatterns: Map[String, Double],
    nonChunkPatterns: Map[String, Double]
  ): Evaluator = {
    new Evaluator(
      leftKeys,
      rightKeys,
      typingPatterns,
      chunkPatterns,
      nonChunkPatterns
    )
  }
}

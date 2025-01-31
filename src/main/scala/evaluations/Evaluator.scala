package layout.evaluations

import scala.collection.mutable.{ Map => MutMap }

import layout.evaluations.types._
import layout.evaluations.values._


object Evaluator {

  def evaluate(keys: String): Double = keys match {
    case "" => 0.0
    case _ =>
      val chunks = partitionedKeysToChunks(partitionKeysByHand(keys))
      chunksToListOfActionChunks(chunks)._2
  }

  def partitionKeysByHand(keys: String): List[(String, Hand)] = {
    def rec(
      keys: List[Char], oneHandKeys: String, lastHand: Hand, partitionedKeys: List[(String, Hand)]
    ): List[(String, Hand)] = keys match {
      case Nil => ((oneHandKeys, lastHand)::partitionedKeys).reverse
      case k::ks if lastHand == keyToHand(k) => rec(ks, oneHandKeys + k, lastHand, partitionedKeys)
      case k::ks => rec(ks, k.toString, keyToHand(k), (oneHandKeys, lastHand)::partitionedKeys)
    }
    keys.toList match {
      case Nil => Nil
      case k::ks => rec(ks, k.toString, keyToHand(k), Nil)
    }
  }

  def partitionedKeysToChunks(partitionedKeys: List[(String, Hand)]): List[HandSpecifiedChunk] = {
    var totalChunks = List[HandSpecifiedChunk]()
    partitionedKeys.foreach { case (keys, hand) =>
      val chunks = keysToChunks(keys, hand, Nil)
      totalChunks ++= chunks
    }
    totalChunks
  }

  def keysToChunks(keys: String, hand: Hand, chunks: List[HandSpecifiedChunk]): List[HandSpecifiedChunk] = keys match {
    case "" => chunks.reverse
    case _ =>
      def rec(takeNumbers: List[Int], keys: String): (String, Int) = takeNumbers match {
        case Nil => throw new RuntimeException("single keys must be defined as ArpeggioStroke")
        case x::xs =>
          val takenKeys = keys.take(x)
          if (getTotalStrokeProperty(Chunk(takenKeys)) == ARPEGGIO_STROKE)
            takenKeys -> x
          else
            rec(xs, keys)
      }
      val (takenKeys, takeNumber) = rec(List(4, 3, 2, 1), keys)
      val chunk = hand match {
        case LeftHand => LeftChunk(takenKeys)
        case RightHand => RightChunk(takenKeys)
      }
      keysToChunks(keys.drop(takeNumber), hand, chunk::chunks)
  }

  def chunksToListOfActionChunks(
    chunks: List[HandSpecifiedChunk], reversed: Boolean = false
  ): (List[List[HandSpecifiedChunk]], Double) = {
    def rec(
      chunks: List[HandSpecifiedChunk],
      leftActionKeyString: OneHandAction, rightActionKeyString: OneHandAction,
      currentActionChunks: List[HandSpecifiedChunk], listOfActionChunks: List[List[HandSpecifiedChunk]],
      score: Double
    ): (List[List[HandSpecifiedChunk]], Double) = chunks match {
      case Nil => (currentActionChunks::listOfActionChunks) -> score
      case x::xs =>
        val (currentActionKeyString, otherActionKeyString) = reverseHands(leftActionKeyString, rightActionKeyString, x)

        val totalStrokeProperty = getTotalStrokeProperty(currentActionKeyString, x)
        val (newCurrentActionKeyString, newOtherActionKeyString, newCurrentActionChunks, newListOfActionChunks) =
          totalStrokeProperty match {
            case `ARPEGGIO_STROKE` =>
              (currentActionKeyString.join(x), otherActionKeyString, x::currentActionChunks, listOfActionChunks)
            case _ => (x.toOneHandAction, OneHandAction(""), x::Nil, currentActionChunks::listOfActionChunks)
          }
        val newScore = score + evaluateTotalStrokeProperty(totalStrokeProperty)

        val (newLeftActionKeyString, newRightActionKeyString) = reverseHands(newCurrentActionKeyString, newOtherActionKeyString, x)
        rec(xs, newLeftActionKeyString, newRightActionKeyString, newCurrentActionChunks, newListOfActionChunks, newScore)
    }
    chunks match {
      case Nil => Nil -> 0.0
      case xs =>
        val (xss, score) = rec(xs, OneHandAction(""), OneHandAction(""), Nil, Nil, 0.0)
        if (reversed) xss.map(_.reverse).reverse -> score else xss -> score
    }
  }

  def evaluateTotalStrokeProperty(totalStrokeProperty: TotalStrokeProperty): Double = totalStrokeProperty match {
    case ChunkableStroke(BasicStrokeEasilyChunkable, HandShapeEasilyChunkable, FingerOrderEasilyChunkable) => 0.0
    case ChunkableStroke(BasicStrokeEasilyChunkable, HandShapeHardChunkable, FingerOrderEasilyChunkable) => 0.7
    case ChunkableStroke(BasicStrokeHardChunkable, HandShapeEasilyChunkable, FingerOrderEasilyChunkable) => 0.7
    case ChunkableStroke(BasicStrokeHardChunkable, HandShapeHardChunkable, FingerOrderEasilyChunkable) => 0.8

    case ChunkableStroke(BasicStrokeEasilyChunkable, HandShapeEasilyChunkable, FingerOrderHardChunkable) => 0.98
    case ChunkableStroke(BasicStrokeEasilyChunkable, HandShapeHardChunkable, FingerOrderHardChunkable) => 0.982
    case ChunkableStroke(BasicStrokeHardChunkable, HandShapeEasilyChunkable, FingerOrderHardChunkable) => 0.982
    case ChunkableStroke(BasicStrokeHardChunkable, HandShapeHardChunkable, FingerOrderHardChunkable) => 0.983

    case ChunkableStroke(BasicStrokeEasilyChunkable, HandShapeImpossiblyHardChunkable, FingerOrderEasilyChunkable) => 0.984
    case ChunkableStroke(BasicStrokeHardChunkable, HandShapeImpossiblyHardChunkable, FingerOrderEasilyChunkable) => 0.985
    case ChunkableStroke(BasicStrokeEasilyChunkable, HandShapeImpossiblyHardChunkable, FingerOrderHardChunkable) => 0.986
    case ChunkableStroke(BasicStrokeHardChunkable, HandShapeImpossiblyHardChunkable, FingerOrderHardChunkable) => 0.987

    case NonChunkableStroke(_) => 1.0
  }

  def reverseHands[T](oneHand: T, otherHand: T, chunk: HandSpecifiedChunk): (T, T) = chunk match {
    case _: LeftChunk => oneHand -> otherHand
    case _: RightChunk => otherHand -> oneHand
  }

  def getTotalStrokeProperty(keys: OneHandActionable): TotalStrokeProperty = getTotalStrokeProperty(OneHandAction(""), keys)

  private val totalStrokePropertyCache: MutMap[(String, String), TotalStrokeProperty] = MutMap()
  def getTotalStrokeProperty(keys1: OneHandActionable, keys2: OneHandActionable): TotalStrokeProperty = {
    lazy val allKeys = OneHandKeyString(keys1 + keys2)
    lazy val basicStrokeProperty = getBasicStrokeProperty(allKeys)
    lazy val handShapeProperty = getHandShapeProperty(allKeys)
    lazy val orderProperty = chunkableOrderOrNot(keys1, keys2)
    lazy val totalStrokeProperty = (basicStrokeProperty, handShapeProperty, orderProperty) match {
      case (BasicStrokeNonChunkable(n), _, _) => NonChunkableStroke(n)
      case (b: Chunkable, h: Chunkable, o: Chunkable) => ChunkableStroke(b, h, o)
    }
    totalStrokePropertyCache.getOrElseUpdate(keys1.keys -> keys2.keys, totalStrokeProperty)
  }

  def getHandShapeProperty(keyString: OneHand): HandShapeProperty =
    allHandShapes.get(getHandShape(keyString)) match {
      case Some(handShapeProperty) => handShapeProperty
      case None => HandShapeEasilyChunkable
    }

  def getHandShape(keyString: OneHand): HandShape = {
    val pairsOfFingerAndYAxis: List[(Finger, Int)] =
      keyString.keys.sortBy(keyCoodinates(_)._1).map(x => keyToFinger(x) -> keyCoodinates(x)._2).toList
    val headFingerPosition = List(pairsOfFingerAndYAxis.head._1 -> SameHeight)
    val tailFingerPositions = pairsOfFingerAndYAxis.tail match {
      case Nil => Nil
      case _ =>
        pairsOfFingerAndYAxis.sliding(2).map {
          case Seq((_, y1), (f2, y2)) => f2 -> getRelativeFingerHeight(y2, y1)
        }
    }
    headFingerPosition ++ tailFingerPositions
  }

  def getRelativeFingerHeight(y2: Int, y1: Int): RelativeFingerHeight =
    if (y2 - y1 == 0)
      SameHeight
    else if (y2 - y1 > 0)
      UpperHeight
    else
      LowerHeight

  def chunkableOrderOrNot(keys1: OneHand, keys2: OneHand): FingerOrderProperty = {
    val pastFingers = keys1.keys.map(keyToFinger(_)).toSet
    val fingers = keys2.keys.map(keyToFinger(_)).toList
    val sameFingerRemoved = pastFingers.filter(!fingers.contains(_))
    def rec(fingers: List[Finger], fingerOrders: List[FingerOrder]): List[FingerOrder] =
      (fingers, fingerOrders) match {
        case (Nil, _) => fingerOrders
        case (finger::restFingers, Nil) => rec(restFingers, (sameFingerRemoved, finger)::Nil)
        case (finger::restFingers, (set, finger2)::_) =>
          val newFingerOrder = (set + finger2, finger)
          rec(restFingers, newFingerOrder::fingerOrders)
      }
    rec(fingers, Nil).map(allFingerOrders.get(_)).flatten match {
      case Nil => FingerOrderEasilyChunkable
      case xs => xs.max
    }
  }

  def getProduct(oneHandKeyString: OneHand): List[BasicStroke] = {
    def rec(keys: String, twoKeyStrokes: List[BasicStroke]): List[BasicStroke] = keys match {
      case "" => twoKeyStrokes
      case keys if keys.length == 1 => twoKeyStrokes
      case keys =>
        val (k1, restKeys) = (keys.head, keys.tail)
        val newTwoStrokes = twoKeyStrokes ++ restKeys.toList.map(k2 => BasicStroke(k1.toString + k2.toString))
        rec(restKeys, newTwoStrokes)
    }
    rec(oneHandKeyString.keys, Nil)
  }

  def getBasicStrokeProperty(keys: OneHand): BasicStrokeProperty = 
    getProduct(keys).map(basicStroke => allStrokePatterns(basicStroke.keys)._1) match {
      case Nil => BasicStrokeEasilyChunkable
      case xs => xs.max
    }

  def clearCaches(): Unit = totalStrokePropertyCache.clear()
}

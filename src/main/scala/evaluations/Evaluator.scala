package layout.evaluations

import scala.collection.mutable.{ Map => MutMap }

import layout.evaluations.types._
import layout.evaluations.values._


object Evaluator {

  def evaluate(keys: String): Double = keys match {
    case "" => 0.0
    case keys =>
      val partitionedKeys = partitionKeysByHand(keys)
      val listOfActionKeys = keysToListOfActionkeys(partitionedKeys)
      val (listOfLeftActionKeys, listOfRightActionKeys) =
        divideListOfActionKeysByHand(listOfActionKeys)
      val leftInterstrokes = getInterstrokes(listOfLeftActionKeys)
      val rightInterstrokes = getInterstrokes(listOfRightActionKeys)
      (leftInterstrokes ++ rightInterstrokes).map(evaluateInterstroke(_)).sum
  }

  def partitionKeysByHand(keys: String): List[(String, Hand)] = {
    def rec(
      keys: List[Char], oneHandKeys: String, lastHand: Option[Hand],
      partitionedKeys: List[(String, Hand)]
    ): List[(String, Hand)] = (keys, lastHand) match {
      case (Nil, None) => partitionedKeys.reverse
      case (Nil, Some(hand)) => ((oneHandKeys, hand)::partitionedKeys).reverse
      case (k::ks, None) => rec(ks, k.toString, Some(keyToHand(k)), Nil)
      case (k::ks, Some(hand)) if hand == keyToHand(k) =>
        rec(ks, oneHandKeys + k, Some(hand), partitionedKeys)
      case (k::ks, Some(hand)) =>
        rec(ks, k.toString, Some(keyToHand(k)), (oneHandKeys, hand)::partitionedKeys)
    }
    rec(keys.toList, "", None, Nil)
  }

  def keysToListOfActionkeys(
    partitionedKeys: List[(String, Hand)]
  ): List[(OneHandAction, Hand)] =
    partitionedKeys.map { case (keys, hand) =>
      keysToChunks(keys, Nil).map(_ -> hand)
    } .flatten

  def divideListOfActionKeysByHand(
    listOfActionKeys: List[(OneHandAction, Hand)]
  ): (List[Option[OneHandAction]], List[Option[OneHandAction]]) = {
    val leftList = listOfActionKeys.map(_ match {
      case (keys, LeftHand) => Some(keys)
      case (_, RightHand) => None
    })
    val rightList = listOfActionKeys.map(_ match {
      case (keys, RightHand) => Some(keys)
      case (_, LeftHand) => None
    })
    (leftList, rightList)
  }

  def getInterstrokes(
    listOfActionKeys: List[Option[OneHandAction]]
  ): List[InterStrokeProperty] = {
    def rec(
      listOfActionKeys: List[Option[OneHandAction]],
      interStrokeProperties: List[InterStrokeProperty]
    ): List[InterStrokeProperty] = listOfActionKeys match {
      case Nil => interStrokeProperties.reverse
      case Some(x)::Some(y)::xs =>
        val interStrokeProperty = getInterStrokeProperty(x, y, AA)
        rec(Some(y)::xs, interStrokeProperty::interStrokeProperties)
      case Some(x)::None::Some(y)::xs =>
        val interStrokeProperty = getInterStrokeProperty(x, y, ABA)
        rec(Some(y)::xs, interStrokeProperty::interStrokeProperties)
      case Some(x)::None::None::Some(y)::xs =>
        val interStrokeProperty = getInterStrokeProperty(x, y, ABBA)
        rec(Some(y)::xs, interStrokeProperty::interStrokeProperties)
      case Some(x)::None::None::None::Some(y)::xs =>
        val interStrokeProperty = getInterStrokeProperty(x, y, ABBBA)
        rec(Some(y)::xs, interStrokeProperty::interStrokeProperties)
      case Some(x)::None::None::None::None::xs =>
        rec(Some(x)::None::None::None::xs, interStrokeProperties)
      case x::xs => rec(xs, interStrokeProperties)
    }
    rec(listOfActionKeys, Nil)
  }

  def getInterStrokeProperty(
    keys1: OneHandAction, keys2: OneHandAction, betweens: HandBsBetweenHandAs
  ): InterStrokeProperty =
    getChunkDifficulty(keys1, keys2) match {
      case property: Joinable => InterJoinable(property, betweens)
      case NonJoinableStroke =>
        val removed = removeNonChunkableCombinationKeys(keys1, keys2).toOneHand
        lazy val nonChunkability = getChunkableTotalStrokeProperty(removed, keys2)
        (removed, nonChunkability, keys1.keys.length, keys2.keys.length) match {
          case (OneHandAction(""), p, len1, len2) if len1 <= len2 =>
            InterNonJoinable(p, betweens)
          case (OneHandAction(""), _, _, _) => InterJustNonJoinable(betweens)
          case (_, p, _, _) => InterNonJoinable(p, betweens)
        }
    }

  def evaluateInterstroke(interchunkability: InterStrokeProperty): Double =
    interchunkability match {
      case InterJoinable(HardFingerOrderStroke, ABBBA) => 0.96
      case InterNonJoinable(HardFingerOrderStroke, ABBBA) => 0.96
      case InterJoinable(HardStroke, ABBBA) => 0.96
      case InterNonJoinable(HardStroke, ABBBA) => 0.96
      case InterJustNonJoinable(ABBBA) => 0.96
      case InterNonJoinable(EasyStroke, ABBBA) => 0.95
      case InterJoinable(EasyStroke, ABBBA) => 0.95

      case InterJoinable(HardFingerOrderStroke, ABBA) => 0.96
      case InterNonJoinable(HardFingerOrderStroke, ABBA) => 0.96
      case InterJoinable(HardStroke, ABBA) => 0.96
      case InterNonJoinable(HardStroke, ABBA) => 0.96
      case InterJustNonJoinable(ABBA) => 0.96
      case InterNonJoinable(EasyStroke, ABBA) => 0.95
      case InterJoinable(EasyStroke, ABBA) => 0.94

      case InterJoinable(HardFingerOrderStroke, _) => 1.0
      case InterNonJoinable(HardFingerOrderStroke, _) => 0.99
      case InterJoinable(HardStroke, _) => 0.98
      case InterNonJoinable(HardStroke, _) => 0.97
      case InterJustNonJoinable(_) => 0.96
      case InterNonJoinable(EasyStroke, _) => 0.95
      case InterJoinable(EasyStroke, _) => 0.0
    }

  def keysToChunks(keys: String, chunks: List[OneHandAction]): List[OneHandAction] = (keys) match {
    case "" => chunks.reverse
    case _ =>
      val chunkCandidates: List[String] = (1 to 4).toList.reverse.map((keys.take(_)))
      val chunk =
        OneHandAction(chunkCandidates.filter(keys =>
          getChunkDifficulty(OneHandAction(keys)) == EasyStroke).head)
      keysToChunks(keys.drop(chunk.keys.length), chunk::chunks)
  }

  def removeNonChunkableCombinationKeys(
    actionKeys: Actionable, chunk: OneHandAction
  ): HandAction =
    HandAction(actionKeys.keys.filter { k1 =>
      val combinations = chunk.keys.map(k2 => k1 -> k2)
      combinations.forall { case (k1, k2) =>
        (keyToHand(k1), keyToHand(k2)) match {
          case (LeftHand, RightHand) => true
          case (RightHand, LeftHand) => true
          case _ => allChunks.get(k1.toString + k2.toString).nonEmpty
        }
      }
    })

  def getChunkableTotalStrokeProperty(
    keys: OneHandActionable
  ): TotalStrokeProperty with Joinable =
    chunkDifficultyToChunkability(getChunkDifficulty(keys))

  def getChunkableTotalStrokeProperty(
    keys1: OneHandActionable, keys2: OneHand
  ): TotalStrokeProperty with Joinable =
    chunkDifficultyToChunkability(getChunkDifficulty(keys1, keys2))

  def chunkDifficultyToChunkability(
    property: TotalStrokeProperty
  ): TotalStrokeProperty with Joinable = property match {
    case property: Joinable => property
    case property => HardStroke
  }

  def getChunkDifficulty(keys: OneHand): TotalStrokeProperty =
    getChunkDifficulty(OneHandAction(""), keys)

  val totalStrokePropertyCache: MutMap[(String, String), TotalStrokeProperty] = MutMap()
  def getChunkDifficulty(keys1: OneHandActionable, keys2: OneHand): TotalStrokeProperty =
    totalStrokePropertyCache.get(keys1.keys -> keys2.keys) match {
      case Some(p) => p
      case None =>
        val allKeys = OneHandAction(keys1 + keys2)

        lazy val productDifficulty = getProductChunkDifficulty(allKeys)
        lazy val orderDifficulty = chunkableOrderOrNot(keys1, keys2)
        lazy val handShapeDifficulty = getHandShapeDifficulty(allKeys)
        val totalStrokeProperty =
          (productDifficulty, handShapeDifficulty, orderDifficulty) match {
            case (NonJoinable, _, _) => NonJoinableStroke
            case (_, NonJoinable, _) => NonJoinableStroke
            case (_, _, NonJoinable) => NonJoinableStroke
            case (HardJoinable, _, _) => HardStroke
            case (_, HardJoinable, _) => HardStroke
            case (EasilyJoinable, EasilyJoinable, HardJoinable) => HardFingerOrderStroke
            case (_, _, HardJoinable) => HardStroke
            case (EasilyJoinable, EasilyJoinable, EasilyJoinable) => EasyStroke
          }
        totalStrokePropertyCache(keys1.keys -> keys2.keys) = totalStrokeProperty
        totalStrokeProperty
    }

  def getHandShapeDifficulty(keyString: OneHandActionable): StrokeProperty =
    allHandShapes.get(getHandShape(keyString)) match {
      case Some(EasilyJoinable) => EasilyJoinable
      case Some(HardJoinable) => HardJoinable
      case Some(NonJoinable) => NonJoinable
      case None => NonJoinable
    }

  def getHandShape(keyString: OneHandActionable): HandShape = {
    val pairsOfFingerAndYAxis: List[(Finger, Int)] =
      keyString.keys.sortBy(keyAxes(_)._1).map(x => keyToFinger(x) -> keyAxes(x)._2).toList
    val headFingerPositions = List(pairsOfFingerAndYAxis.head._1 -> SameHeight)
    val tailFingerPositions = pairsOfFingerAndYAxis.tail match {
      case Nil => Nil
      case _ =>
      pairsOfFingerAndYAxis.sliding(2).map {
        case Seq((_, y1), (f2, y2)) => f2 -> getRelativeFingerHeight(y2, y1)
      }
    }
    headFingerPositions ++ tailFingerPositions
  }

  def getRelativeFingerHeight(y2: Int, y1: Int): RelativeFingerHeight =
    if (y2 - y1 == 0)
      SameHeight
    else if (y2 - y1 > 0)
      UpperHeight
    else
      LowerHeight

  def chunkableOrderOrNot(keys1: OneHand, keys2: OneHand): StrokeProperty = {
    val pastFingers = keys1.keys.map(keyToRealFinger(_)).toSet
    val fingers = keys2.keys.map(keyToRealFinger(_)).toList
    val sameFingerRemoved = pastFingers.filter(!fingers.contains(_))
    def rec(fingers: List[Finger], fingerOrders: List[FingerOrder]): List[FingerOrder] =
      (fingers, fingerOrders) match {
        case (Nil, _) => fingerOrders
        case (finger::restFingers, Nil) => rec(restFingers, (sameFingerRemoved, finger)::Nil)
        case (finger::restFingers, (set, finger2)::_) =>
          val newFingerOrder = (set + finger2, finger)
          rec(restFingers, newFingerOrder::fingerOrders)
      }
    val result = rec(fingers, Nil).map(allFingerOrders.get(_))
    if (result.contains(None))
      NonJoinable
    else if (result.contains(Some(HardJoinable)))
      HardJoinable
    else
      EasilyJoinable
  }

  def getProduct(oneHandKeyString: OneHand): List[OneHandAction] = {
    def rec(keys: String, twoStrokes: List[OneHandAction]): List[OneHandAction] = (keys) match {
      case "" => twoStrokes
      case keys if keys.length == 1 => twoStrokes
      case keys =>
        val (k1, restKeys) = (keys.head, keys.tail)
        val newTwoStrokes =
          twoStrokes ++
          (for (k2 <- restKeys) yield OneHandAction(k1.toString + k2.toString)).toList
        rec(restKeys, newTwoStrokes)
    }
    rec(oneHandKeyString.keys, Nil)
  }

  def getProductChunkDifficulty(keys: OneHandAction): StrokeProperty = {
    val chunkDifficulties = getProduct(keys).map(unitKeyString => allStrokePatterns(unitKeyString.keys)._1)
    if (chunkDifficulties.contains(NonJoinable))
      NonJoinable
    else if (chunkDifficulties.contains(HardJoinable))
      HardJoinable
    else
      EasilyJoinable
  }
}

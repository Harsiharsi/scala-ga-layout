package layout.evaluations


package object types {
  import scala.math.Ordered

  sealed trait RelativeFingerHeight
  case object SameHeight extends RelativeFingerHeight
  case object UpperHeight extends RelativeFingerHeight
  case object LowerHeight extends RelativeFingerHeight

  type FingerPosition = (Finger, RelativeFingerHeight)
  type HandShape = List[FingerPosition]

  sealed trait Hand
  case object LeftHand extends Hand
  case object RightHand extends Hand


  sealed trait Joinable
  sealed trait StrokeProperty
  case object EasilyJoinable extends StrokeProperty with Joinable
  case object HardJoinable extends StrokeProperty with Joinable
  case object NonJoinable extends StrokeProperty

  sealed trait TotalStrokeProperty
  case object EasyStroke extends TotalStrokeProperty with Joinable
  case object HardStroke extends TotalStrokeProperty with Joinable
  case object HardFingerOrderStroke extends TotalStrokeProperty with Joinable
  case object NonJoinableStroke extends TotalStrokeProperty

  sealed trait HandBsBetweenHandAs
  case object AA extends HandBsBetweenHandAs
  case object ABA extends HandBsBetweenHandAs
  case object ABBA extends HandBsBetweenHandAs
  case object ABBBA extends HandBsBetweenHandAs
  sealed trait InterStrokeProperty {
    val betweens: HandBsBetweenHandAs
  }
  case class InterJoinable(
    val strokeProperty: TotalStrokeProperty with Joinable,
    val betweens: HandBsBetweenHandAs
  ) extends InterStrokeProperty
  case class InterNonJoinable(
    val strokeProperty: TotalStrokeProperty with Joinable,
    val betweens: HandBsBetweenHandAs
  ) extends InterStrokeProperty
  case class InterJustNonJoinable(
    val betweens: HandBsBetweenHandAs
  ) extends InterStrokeProperty

  sealed trait Finger extends Ordered[Finger] {
    def compare(y: Finger): Int = (this, y) match {
      case (x, y) if (x eq y) => 0
      case (StretchedIndex, _) => -1
      case (_, StretchedIndex) => 1
      case (Index, _) => -1
      case (_, Index) => 1
      case (Middle, _) => -1
      case (_, Middle) => 1
      case (Ring, _) => -1
      case (_, Ring) => 1
      case (StretchedRing, _) => -1
      case (_, StretchedRing) => 1
      case (Pinky, _) => -1
      case (_, Pinky) => 1
      case (StretchedPinky, _) => -1
      case (_, StretchedPinky) => 1
    }
  }
  case object Thumb extends Finger
  case object StretchedIndex extends Finger
  case object Index extends Finger
  case object Middle extends Finger
  case object Ring extends Finger
  case object StretchedRing extends Finger
  case object Pinky extends Finger
  case object StretchedPinky extends Finger

  type FingerOrder = (Set[Finger], Finger)


  sealed trait KeyString {
    val keys: String
    def +(s: String): String = keys + s
    def +(kstr: KeyString): String = keys + kstr.keys
    override def toString = keys
  }
  sealed trait OneHand extends KeyString
  sealed trait Actionable extends KeyString
  sealed trait StraightOrdered extends KeyString
  sealed trait MultiChunk extends KeyString

  type BothHandActionable = Actionable
  type OneHandActionable = OneHand with Actionable

  case class Chunk(val keys: String) extends OneHand with Actionable {
    def toHandAction: HandAction = HandAction(keys)
    def toOneHandAction: OneHandAction = OneHandAction(keys)
  }
  case class HandAction(val keys: String) extends Actionable with MultiChunk {
    def toOneHand: OneHandAction = OneHandAction(keys)
  }
  case class OneHandAction(val keys: String) extends OneHand with Actionable with MultiChunk


  val keyToFingerAndHand: Map[Char, (Hand, Finger)] = Map(
    '1' -> (LeftHand, StretchedRing), '2' -> (LeftHand, Ring), '3' -> (LeftHand, Middle),
    '4' -> (LeftHand, Index), '5' -> (LeftHand, StretchedIndex),

    'q' -> (LeftHand, StretchedRing), 'w' -> (LeftHand, Ring), 'e' -> (LeftHand, Middle),
    'r' -> (LeftHand, Index), 't' -> (LeftHand, StretchedIndex),

    'a' -> (LeftHand, Pinky), 's' -> (LeftHand, Ring), 'd' -> (LeftHand, Middle),
    'f' -> (LeftHand, Index), 'g' -> (LeftHand, StretchedIndex),

    'z' -> (LeftHand, Pinky), 'x' -> (LeftHand, Ring), 'c' -> (LeftHand, Middle),
    'v' -> (LeftHand, Index), 'b' -> (LeftHand, StretchedIndex),


    '6' -> (RightHand, StretchedIndex), '7' -> (RightHand, Index), '8' -> (RightHand, Middle),
    '9' -> (RightHand, Ring), '0' -> (RightHand, StretchedRing),

    'y' -> (RightHand, StretchedIndex), 'u' -> (RightHand, Index), 'i' -> (RightHand, Middle),
    'o' -> (RightHand, Ring), 'p' -> (RightHand, StretchedRing),
    '[' -> (RightHand, StretchedPinky),

    'h' -> (RightHand, StretchedIndex), 'j' -> (RightHand, Index), 'k' -> (RightHand, Middle),
    'l' -> (RightHand, Ring), ';' -> (RightHand, Pinky),
    '\'' -> (RightHand, StretchedPinky),

    'n' -> (RightHand, StretchedIndex), 'm' -> (RightHand, Index), ',' -> (RightHand, Middle),
    '.' -> (RightHand, Ring), '/' -> (RightHand, Pinky)
  )
  val keyToFinger: Map[Char, Finger] = keyToFingerAndHand.map { case (c, (_, f)) => c -> f }
  val keyToHand: Map[Char, Hand] = keyToFingerAndHand.map { case (c, (h, _)) => c -> h }
  val keyToRealFinger: Map[Char, Finger] = keyToFinger.map(_ match {
    case (x, StretchedRing) => x -> Ring
    case (x, StretchedIndex) => x -> Index
    case (x, y) => x -> y
  })

  val keyAxes: Map[Char, (Int, Int)] = Map(
    '1' -> (1, 4), '2' -> (2, 4), '3' -> (3, 4), '4' -> (4, 4), '5' -> (5, 4),
    'q' -> (1, 3), 'w' -> (2, 3), 'e' -> (3, 3), 'r' -> (4, 3), 't' -> (5, 3),
    'a' -> (1, 2), 's' -> (2, 2), 'd' -> (3, 2), 'f' -> (4, 2), 'g' -> (5, 2),
    'z' -> (1, 1), 'x' -> (2, 1), 'c' -> (3, 1), 'v' -> (4, 1), 'b' -> (5, 1),

    '6' -> (5, 4), '7' -> (4, 4), '8' -> (3, 4), '9' -> (2, 4), '0' -> (1, 4),
    'y' -> (5, 3), 'u' -> (4, 3), 'i' -> (3, 3), 'o' -> (2, 3), 'p' -> (1, 3),
    '[' -> (0, 3),
    'h' -> (5, 2), 'j' -> (4, 2), 'k' -> (3, 2), 'l' -> (2, 2), ';' -> (1, 2),
    '\'' -> (0, 2),
    'n' -> (5, 1), 'm' -> (4, 1), ',' -> (3, 1), '.' -> (2, 1), '/' -> (1, 1)
  )
}

package object values {
  import layout.evaluations.types._
  import layout.evaluations.fileLoader._

  val allFingerOrders: Map[FingerOrder, StrokeProperty] = loadFingerOrders()
  val allHandShapes: Map [HandShape, StrokeProperty] = loadHandShapes()

  val allStrokePatterns: Map[String, (StrokeProperty, Hand)] = loadTwoStrokePatterns
  val allChunks: Map[String, (StrokeProperty, Hand)] = allStrokePatterns.filter(t => t._2._1 != NonJoinable)
  val allNonChunks: Map[String, (StrokeProperty, Hand)] = allStrokePatterns.filter(t => t._2._1 == NonJoinable)
}

package object fileLoader {
  import scala.io.{Source => S}

  import layout.evaluations.types._
  import layout.evaluations.Evaluator._


  def stringToFinger(s: String): Finger = (s) match {
    case "T" => Thumb
    case "SI" => StretchedIndex
    case "I" => Index
    case "M" => Middle
    case "R" => Ring
    case "SR" => StretchedRing
    case "P" => Pinky
    case "SP" => StretchedPinky
  }

  def stringToChunkDifficulty(s: String): StrokeProperty = s match {
    case "EasilyJoinable" => EasilyJoinable
    case "HardJoinable" => HardJoinable
    case "NonJoinable" => NonJoinable
  }
  def loadHandShapes(): Map[HandShape, StrokeProperty] = {
    val file = S.fromFile("./src/main/resources/typingdatas/hand_shapes.txt", "utf-8")
    val m = (for (l <- file.getLines if l.nonEmpty && l(0) != '#') yield {
      val splits = l.split('\t')
      val (column1, column2) = (splits(0), splits(1))
      val handShape = column1.split('-').map { x =>
        val finger = stringToFinger(x.take(x.length - 1))
        val relativeFingerHeight = (x.last) match {
          case 's' => SameHeight
          case 'u' => UpperHeight
          case 'l' => LowerHeight
        }
        (finger, relativeFingerHeight)
      } .toList
      val chunkDifficulty = stringToChunkDifficulty(column2)
      handShape -> chunkDifficulty
    }).toMap
    file.close

    m.toMap
  }

  def loadFingerOrders(): Map[FingerOrder, StrokeProperty] = {
    val file = S.fromFile("./src/main/resources/typingdatas/finger_orders.txt", "utf-8")
    val m = (for (l <- file.getLines if l.nonEmpty && l(0) != '#') yield {
      val splits = l.split('\t')
      val (column1, column2, column3) = (splits(0), splits(1), splits(2))
      val fingerOrder = {
        val strokedFingers = column1.split('-').map(stringToFinger(_)).toSet
        val nextFinger = stringToFinger(column2)
        strokedFingers -> nextFinger
      }
      val chunkDifficulty = stringToChunkDifficulty(column3)
      fingerOrder -> chunkDifficulty
    }).toMap
    file.close

    m ++ List(
      Thumb, StretchedIndex, Index, Middle,
      Ring, StretchedRing,
      Pinky, StretchedPinky).map(finger => (Set[Finger](), finger) -> EasilyJoinable).toMap
  }

  def loadTwoStrokePatterns(): Map[String, (StrokeProperty, Hand)] = {
    val file = S.fromFile("src/main/resources/typingdatas/stroke_patterns.txt", "utf-8")
    val m = (for (l <- file.getLines if l.nonEmpty && l(0) != '#') yield {
      val splits = l.split('\t')
      val (column1, column2) = (splits(0), splits(1))
      val strokePattern = column1
      val chunkDifficulty = stringToChunkDifficulty(column2)
      val hand = keyToHand(strokePattern.head)
      strokePattern -> (chunkDifficulty, hand)
    }).toMap
    file.close

    m
  }
}

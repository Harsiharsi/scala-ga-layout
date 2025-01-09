package layout.layouts

import scala.util.{Random => R}
import scala.io.{Source => S}
import scala.math.{abs, pow, sqrt}
import scala.reflect.ClassTag

import layout.layouts.LayoutPhonemeTypes._


object LayoutPhonemeTypes {
  sealed trait Hand
  sealed trait LeftHand extends Hand
  sealed trait RightHand extends Hand
  type BothHand = LeftHand with RightHand

  case class PhysicalKey[H >: BothHand](val key: String)

  sealed trait Occupant[H >: BothHand]
  sealed trait DoubleOccupant[H >: BothHand] extends Occupant[H] { val char: String }
  case class SingleOccupant[H >: BothHand](
    val frontChar: OneHandSingleOccupantChar[H], val backChar: OneHandSingleOccupantChar[H]
  ) extends Occupant[H]

  sealed trait AssignableThing
  sealed trait AssignableChar extends AssignableThing { val char: String }
  sealed trait DoubleOccupantChar[H >: BothHand] extends AssignableChar with DoubleOccupant[H] { val char: String }
  sealed trait SingleOccupantChar extends AssignableChar
  type LeftSingleOccupantChar = SingleOccupantChar with LeftHand
  type RightSingleOccupantChar = SingleOccupantChar with RightHand
  type OneHandSingleOccupantChar[H >: BothHand] = SingleOccupantChar with H

  sealed trait RowChar

  sealed trait Frequent
  case object Shift extends AssignableThing with DoubleOccupant[LeftHand] with Frequent { val char = " S" }
  case object Dakuten extends AssignableThing with DoubleOccupant[RightHand] with Frequent { val char = "゛" }
  case object Handakuten extends AssignableThing with DoubleOccupant[LeftHand] { val char = "゜" }

  case class KRow(val char: String) extends SingleOccupantChar with LeftHand with RowChar
  case class SRow(val char: String) extends SingleOccupantChar with LeftHand with RowChar
  case class TRow(val char: String) extends SingleOccupantChar with LeftHand with RowChar
  case class HRow(val char: String) extends SingleOccupantChar with LeftHand with RowChar

  case class NRow(val char: String) extends SingleOccupantChar with RightHand with RowChar
  case class MRow(val char: String) extends SingleOccupantChar with RightHand with RowChar
  case class RRow(val char: String) extends SingleOccupantChar with RightHand with RowChar
  case class WRow(val char: String) extends SingleOccupantChar with RightHand with RowChar
  case class ARow(val char: String) extends DoubleOccupantChar[RightHand] with RightHand with RowChar
  case class YRow(val char: String) extends DoubleOccupantChar[RightHand] with RightHand with RowChar

  case class OtherChar(val char: String) extends SingleOccupantChar with LeftHand with RightHand
  case object EmptyChar extends SingleOccupantChar with LeftHand with RightHand { val char = " *" }

  type LeftChromosome = Map[PhysicalKey[LeftHand], Occupant[LeftHand]]
  type RightChromosome = Map[PhysicalKey[RightHand], Occupant[RightHand]]
  type BothChromosome = (LeftChromosome, RightChromosome)
  type OneHandChromosome[H >: BothHand] = Map[PhysicalKey[H], Occupant[H]]
  type TemporalChromosome[H >: BothHand] = Map[PhysicalKey[H], Option[Occupant[H]]]

  type CharToKeyMap = Map[String, String]

  type Coordinate = (Int, Int)
}

case class LayoutPhoneme(val leftChromosome: LeftChromosome, val rightChromosome: RightChromosome) extends Layout {
  import layout.layouts.functions._
  import layout.layouts.LayoutPhoneme._

  type T = LayoutPhoneme

  private var privateFitness: Option[Double] = None
  private var privateStrokeFitness: Option[Double] = None
  private var privateMemorabilityFitness: Option[Double] = None

  def fitness: Double = privateFitness match {
    case Some(n) => n
    case None =>
      evaluate
      privateFitness.get
  }

  def strokeFitness: Double = privateStrokeFitness match {
    case Some(n) => n
    case None =>
      evaluate
      privateStrokeFitness.get
  }

  def memorabilityFitness: Double = privateMemorabilityFitness match {
    case Some(n) => n
    case None =>
      evaluate
      privateMemorabilityFitness.get
  }

  def isEvaluated: Boolean = privateFitness.isDefined

  def mutate(): LayoutPhoneme = {
    val (l, r) = if (R.nextBoolean) {
      val (l, r) = shuffleChromosome(leftChromosome, rightChromosome)
      l -> r
    } else {
      val (r, l) = shuffleChromosome(rightChromosome, leftChromosome)
      l -> r
    }
    new LayoutPhoneme(l, r)
  }

  def evaluate: LayoutPhoneme = evaluateOnlyStrokeFitness // evaluateCompletely

  def evaluateCompletely: LayoutPhoneme = {
    val newStrokeFitness = evaluateFitness(keyCharToCharKey(leftChromosome, rightChromosome), ngrams)
    val newMemorabilityFitness = evaluateArrangementMemorability(leftChromosome, rightChromosome)
    privateFitness = Some(newStrokeFitness * newMemorabilityFitness)
    privateStrokeFitness = Some(newStrokeFitness)
    privateMemorabilityFitness = Some(newMemorabilityFitness)
    this
  }

  def evaluateOnlyStrokeFitness: LayoutPhoneme = {
    val newStrokeFitness = evaluateFitness(keyCharToCharKey(leftChromosome, rightChromosome), ngrams)
    val newMemorabilityFitness = 0.0
    privateFitness = Some(newStrokeFitness)
    privateStrokeFitness = Some(newStrokeFitness)
    privateMemorabilityFitness = Some(0.0)
    this
  }

  override def toString: String = {
    chromosomeToString(leftChromosome, rightChromosome) ++ "\n" ++
    fitness.toString ++ "  total fitness" ++ "\n" ++
    memorabilityFitness.toString ++ "  memorability" ++ "\n" ++
    strokeFitness.toString ++ "  stroke fitness"
  }
}

object LayoutPhoneme extends LayoutCompanion {
  import layout.layouts.values._
  import layout.evaluations.values.allNonChunks

  type T = LayoutPhoneme

  val leftKeys: List[PhysicalKey[LeftHand]] = List(
    PhysicalKey("1"), PhysicalKey("2"), PhysicalKey("3"), PhysicalKey("4"), PhysicalKey("5"),
    PhysicalKey("q"), PhysicalKey("w"), PhysicalKey("e"), PhysicalKey("r"), PhysicalKey("t"),
    PhysicalKey("a"), PhysicalKey("s"), PhysicalKey("d"), PhysicalKey("f"), PhysicalKey("g"),
    PhysicalKey("z"), PhysicalKey("x"), PhysicalKey("c"), PhysicalKey("v"), PhysicalKey("b")
  )
  val rightKeys: List[PhysicalKey[RightHand]] = List(
                      PhysicalKey("7"), PhysicalKey("8"), PhysicalKey("9"), PhysicalKey("0"),
    PhysicalKey("y"), PhysicalKey("u"), PhysicalKey("i"), PhysicalKey("o"), PhysicalKey("p"),
    PhysicalKey("h"), PhysicalKey("j"), PhysicalKey("k"), PhysicalKey("l"), PhysicalKey(";"),
    PhysicalKey("n"), PhysicalKey("m"), PhysicalKey(","), PhysicalKey("."), PhysicalKey("/")
  )
  val keyCoordinates: Map[PhysicalKey[_], (Int, Int)] = Map(
    PhysicalKey[LeftHand]("1") -> (1, 4), PhysicalKey[LeftHand]("2") -> (2, 4), PhysicalKey[LeftHand]("3") -> (3, 4),
    PhysicalKey[LeftHand]("4") -> (4, 4), PhysicalKey[LeftHand]("5") -> (5, 4),

    PhysicalKey[LeftHand]("q") -> (1, 3), PhysicalKey[LeftHand]("w") -> (2, 3), PhysicalKey[LeftHand]("e") -> (3, 3),
    PhysicalKey[LeftHand]("r") -> (4, 3), PhysicalKey[LeftHand]("t") -> (5, 3),

    PhysicalKey[LeftHand]("a") -> (1, 2), PhysicalKey[LeftHand]("s") -> (2, 2), PhysicalKey[LeftHand]("d") -> (3, 2),
    PhysicalKey[LeftHand]("f") -> (4, 2), PhysicalKey[LeftHand]("g") -> (5, 2),

    PhysicalKey[LeftHand]("z") -> (1, 1), PhysicalKey[LeftHand]("x") -> (2, 1), PhysicalKey[LeftHand]("c") -> (3, 1),
    PhysicalKey[LeftHand]("v") -> (4, 1), PhysicalKey[LeftHand]("b") -> (5, 1),

    PhysicalKey[RightHand]("6") -> (5, 4), PhysicalKey[RightHand]("7") -> (4, 4), PhysicalKey[RightHand]("8") -> (3, 4),
    PhysicalKey[RightHand]("9") -> (2, 4), PhysicalKey[RightHand]("0") -> (1, 4),

    PhysicalKey[RightHand]("y") -> (5, 3), PhysicalKey[RightHand]("u") -> (4, 3), PhysicalKey[RightHand]("i") -> (3, 3),
    PhysicalKey[RightHand]("o") -> (2, 3), PhysicalKey[RightHand]("p") -> (1, 3), PhysicalKey[RightHand]("[") -> (0, 3),

    PhysicalKey[RightHand]("h") -> (5, 2), PhysicalKey[RightHand]("j") -> (4, 2), PhysicalKey[RightHand]("k") -> (3, 2),
    PhysicalKey[RightHand]("l") -> (2, 2), PhysicalKey[RightHand](";") -> (1, 2), PhysicalKey[RightHand]("\'") -> (0, 2),

    PhysicalKey[RightHand]("n") -> (5, 1), PhysicalKey[RightHand]("m") -> (4, 1), PhysicalKey[RightHand](",") -> (3, 1),
    PhysicalKey[RightHand](".") -> (2, 1), PhysicalKey[RightHand]("/") -> (1, 1)
  )
  val allKeys: List[PhysicalKey[_ >: BothHand]] = leftKeys ++ rightKeys
  val frontKeyToBackKey: Map[String, String] =
    ("12345qwertasdfgzxcvb67890yuiop[hjkl;\'nm,./" ++ "!@#$%QWERTASDFGZXCVB^&*()YUIOP{HJKL:\"NM<>?")
      .zip("!@#$%QWERTASDFGZXCVB^&*()YUIOP{HJKL:\"NM<>?" ++ "!@#$%QWERTASDFGZXCVB^&*()YUIOP{HJKL:\"NM<>?")
      .map { case (k1, k2) => k1.toString -> k2.toString } .toMap
  val backKeyToFrontKey: Map[String, String] =
    ("12345qwertasdfgzxcvb67890yuiop[hjkl;\'nm,./" ++  "!@#$%QWERTASDFGZXCVB^&*()YUIOP{HJKL:\"NM<>?")
      .zip("12345qwertasdfgzxcvb67890yuiop[hjkl;\'nm,./" ++ "12345qwertasdfgzxcvb67890yuiop[hjkl;\'nm,./")
      .map { case (k1, k2) => k1.toString -> k2.toString } .toMap

  val kRowChars = List(KRow("か"), KRow("き"), KRow("く"), KRow("け"), KRow("こ"))
  val sRowChars = List(SRow("さ"), SRow("し"), SRow("す"), SRow("せ"), SRow("そ"))
  val tRowChars = List(TRow("た"), TRow("ち"), TRow("つ"), TRow("て"), TRow("と"))
  val hRowChars = List(HRow("は"), HRow("ひ"), HRow("ふ"), HRow("へ"), HRow("ほ"))
  val aRowChars = List(ARow("あ"), ARow("い"), ARow("う"), ARow("え"), ARow("お"))
  val nRowChars = List(NRow("な"), NRow("に"), NRow("ぬ"), NRow("ね"), NRow("の"))
  val mRowChars = List(MRow("ま"), MRow("み"), MRow("む"), MRow("め"), MRow("も"))
  val yRowChars = List(YRow("ゃ"), YRow("ゅ"), YRow("ょ"))
  val rRowChars = List(RRow("ら"), RRow("り"), RRow("る"), RRow("れ"), RRow("ろ"))
  val wRowChars = List(WRow("わ"), WRow("を"))
  val otherChars = List(OtherChar("ん"), OtherChar("っ"), OtherChar("ー"), OtherChar("、"), OtherChar("。"))

  val leftSingleOccupantChars = kRowChars ++ sRowChars ++ tRowChars ++ hRowChars
  val rightSingleOccupantChars = nRowChars ++ mRowChars ++ rRowChars ++ wRowChars
  val leftDoubleOccupants = List(Shift, Handakuten)
  val rightDoubleOccupants = aRowChars ++ yRowChars ++ List(Dakuten)
  val leftAssignableThings = leftSingleOccupantChars ++ leftDoubleOccupants
  val rightAssignableThings = rightSingleOccupantChars ++ rightDoubleOccupants
  val allAssignableThing = leftAssignableThings ++ rightAssignableThings ++ otherChars

  val vowelsAndYrow = "あいうえおゃゅょ".map(_.toString).toList
  val shiftedVowelsAndYrow = "ぁぃぅぇぉやゆよ".map(_.toString).toList
  val vowelsToShifted: Map[String, String] = vowelsAndYrow.zip(shiftedVowelsAndYrow).toMap
  val vu = List("ヴ")
  val voiceless = "かきくけこさしすせそたちつてとはひふへほ".map(_.toString).toList
  val voiced = "がぎぐげござじずぜぞだぢづでどばびぶべぼ".map(_.toString).toList
  val voicelessToVoiced: Map[String, String] = voiceless.zip(voiced).toMap
  val hRow = "はひふへほ".map(_.toString).toList
  val pRow = "ぱぴぷぺぽ".map(_.toString).toList
  val hToP: Map[String, String] = hRow.zip(pRow).toMap
  val simpleChars = "なにぬねのまみむめもらりるれろわをっんー".map(_.toString).toList
  val ten = List("、")
  val maru = List("。")
  val allCharacters = vowelsAndYrow ++ shiftedVowelsAndYrow ++ vu ++ voiceless ++ voiced ++ pRow ++ simpleChars ++ ten ++ maru

  val ngrams: Map[String, Int] = fourGrams
  val singleOccupantCharFreqs: Map[String, Int] = getSingleOccupantCharFreqs


  def apply(): LayoutPhoneme = {
    val (l, r) = makeChromosome
    new LayoutPhoneme(l, r)
  }

  def apply(leftChromosome: LeftChromosome, rightChromosome: RightChromosome): LayoutPhoneme = {
    new LayoutPhoneme(leftChromosome, rightChromosome)
  }

  def putDoubleOccupants[H >: BothHand](
    doubleOccupants: List[DoubleOccupant[H]], chromosome: TemporalChromosome[H]
  ): TemporalChromosome[H] = {
    val keys = chromosome.collect { case (k, None) => k } .toList
    chromosome ++ R.shuffle(keys).zip(R.shuffle(doubleOccupants.map(Some(_)))).toMap
  }

  def putSingleOccupantChars[H >: BothHand](
    singleOccupantChars: List[OneHandSingleOccupantChar[H]], chromosome: TemporalChromosome[H]
  ): TemporalChromosome[H] = {
    val keys: List[String] = {
      val frontKeys = chromosome.collect { case (physicalKey, None) => physicalKey.key } .toList
      R.shuffle(frontKeys ++ frontKeys.map(frontKeyToBackKey(_)))
    }
    val keysWithEmptyChars: Map[String, OneHandSingleOccupantChar[H]] = keys.map(_ -> EmptyChar).toMap
    val keysWithChars: Map[String, OneHandSingleOccupantChar[H]] =
      keysWithEmptyChars ++ keys.zip(R.shuffle(singleOccupantChars)).toMap
    val groupedKeys: Map[PhysicalKey[H], List[(PhysicalKey[H], OneHandSingleOccupantChar[H])]] =
      keysWithChars.toList.map { case (k, v) =>
        PhysicalKey[H](backKeyToFrontKey(k)) -> v
      } .groupBy { case (k, v) => k }
    val singleOccupantCharsPut: Map[PhysicalKey[H], Option[SingleOccupant[H]]] =
      groupedKeys.toList.map { case (k1, vs) =>
        val frontAndBackChars: List[OneHandSingleOccupantChar[H]] = vs.map { case (k2, v2) => v2 }
        val frontChar: OneHandSingleOccupantChar[H] = frontAndBackChars.head
        val backChar: OneHandSingleOccupantChar[H] = frontAndBackChars.last
        k1 -> Some(SingleOccupant[H](frontChar, backChar))
      } .toMap
    chromosome ++ singleOccupantCharsPut
  }

  def makeChromosome: BothChromosome = {
    val otherCharsTakeNumber = R.shuffle(0 to 3).head
    val rightOtherChars: List[RightSingleOccupantChar] = otherChars.take(otherCharsTakeNumber)
    val leftOtherChars: List[LeftSingleOccupantChar] = otherChars.drop(otherCharsTakeNumber)

    val leftTemporalChromosome: TemporalChromosome[LeftHand] = leftKeys.map(_ -> None).toMap
    val leftDoubleOccupantsPut = putDoubleOccupants(leftDoubleOccupants, leftTemporalChromosome)
    val leftSingleOccupantsPut = putSingleOccupantChars(leftSingleOccupantChars ++ leftOtherChars, leftDoubleOccupantsPut)
    val leftChromosome = leftSingleOccupantsPut.map {
      case (k, Some(v)) => k -> v
      case (_, None) => throw new RuntimeException("something went wrong")
    }
    val rightTemporalChromosome: TemporalChromosome[RightHand] = rightKeys.map(_ -> None).toMap
    val rightDoubleOccupantsPut = putDoubleOccupants(rightDoubleOccupants, rightTemporalChromosome)
    val rightSingleOccupantsPut = putSingleOccupantChars(rightSingleOccupantChars ++ rightOtherChars, rightDoubleOccupantsPut)
    val rightChromosome = rightSingleOccupantsPut.map {
      case (k, Some(v)) => k -> v
      case (_, None) => throw new RuntimeException("something went wrong")
    }
    optimizeChromosome(leftChromosome) -> optimizeChromosome(rightChromosome)
  }

  def optimizeChromosome[H >: BothHand](chromosome: OneHandChromosome[H]): OneHandChromosome[H] = {
    def frequnetCharsToFront(chromosome: OneHandChromosome[H]) = chromosome.map(_ match {
      case (k, x: DoubleOccupant[H]) => k -> x
      case (k, SingleOccupant(c1, c2)) if singleOccupantCharFreqs(c1.char) >= singleOccupantCharFreqs(c2.char) =>
        k -> SingleOccupant(c1, c2)
      case (k, SingleOccupant(c1, c2)) if singleOccupantCharFreqs(c1.char) < singleOccupantCharFreqs(c2.char) =>
        k -> SingleOccupant(c2, c1)
    })
    def fillEmptyFrontKeys(chromosome: OneHandChromosome[H]) = {
      val genesHaveNoFrontChars = R.shuffle(chromosome.filter(_ match {
        case (_, SingleOccupant(EmptyChar, _)) => true
        case _ => false
      }).toList)
      val genesHaveBackChars = {
        val genes = R.shuffle(chromosome.filter {
          case (_, SingleOccupant(EmptyChar, _)) => false
          case (_, SingleOccupant(_, EmptyChar)) => false
          case (_, _: DoubleOccupant[H]) => false
          case (_, SingleOccupant(_, _)) => true
        } .toList)
        val notPunctuations = genes.filter {
          case (_, SingleOccupant(_, OtherChar("、"))) => false
          case (_, SingleOccupant(_, OtherChar("。"))) => false
          case _ => true
        }
        notPunctuations ++ genes.diff(notPunctuations)
      }
      val (givenFrontChars, gaveBackChars) = genesHaveNoFrontChars.zip(genesHaveBackChars).map(_ match {
        case ((k1, SingleOccupant(charA, charB)), (k2, SingleOccupant(charC, charD))) =>
          (k1, SingleOccupant(charD, charB)) -> (k2, SingleOccupant(charC, charA))
        case x => x
      }).unzip
      chromosome ++ givenFrontChars.toMap ++ gaveBackChars.toMap
    }
    def moveKeyFromPinkyToOtherFinger(chromosome: OneHandChromosome[H]) = {
      val pinkyKeys = "az;/".toList.map(_.toString)
      val frequentOccupantsOnPinkies = chromosome.toList.filter {
        case (PhysicalKey(k), o: Frequent) if pinkyKeys.contains(k) => true
        case _ => false
      }
      val otherOccupants = chromosome.toList.filterNot(frequentOccupantsOnPinkies.contains(_))
      val (chromosome1, chromosome2) = R.shuffle(frequentOccupantsOnPinkies).zip(R.shuffle(otherOccupants)).map {
        case ((pk1, o1), (pk2, o2)) => (pk1, o2) -> (pk2, o1)
      } .unzip
      chromosome ++ chromosome1.toMap ++ chromosome2.toMap
    }

    val filled = fillEmptyFrontKeys(frequnetCharsToFront(chromosome))
    moveKeyFromPinkyToOtherFinger(filled)
  }

  def shuffleChromosome[H >: BothHand, I >: BothHand](
    chromosome: OneHandChromosome[H], otherChromosome: OneHandChromosome[I]
  ): (OneHandChromosome[H], OneHandChromosome[I]) = {
    def shuffleOneChromosome[H >: BothHand](
      chromosome: OneHandChromosome[H], k1: PhysicalKey[H],
      charA: OneHandSingleOccupantChar[H], charB: OneHandSingleOccupantChar[H],
      shuffledGenes: List[(PhysicalKey[H], Occupant[H])]
    ): OneHandChromosome[H] = {
      val (k2, o2) = shuffledGenes.collect { case (k2, o2: SingleOccupant[H]) if k2 != k1 => k2 -> o2 } .head
      val (charC, charD) = o2.frontChar -> o2.backChar
      val (shuffledPair1, shuffledPair2)= R.shuffle(List(charA, charB)) -> R.shuffle(List(charC, charD))
      val (newCharA, newCharB, newCharC, newCharD) = if (R.nextDouble < 0.66)
        (shuffledPair1.head, shuffledPair2.head, shuffledPair1.last, shuffledPair2.last)
      else
        (charC, charD, charA, charB)
      val (newOccupant1, newOccupant2) =
        SingleOccupant(newCharA, newCharB) -> SingleOccupant(newCharC, newCharD)
      chromosome ++ Map(k1 -> newOccupant1, k2 -> newOccupant2)
    }
    def shuffleForOtherChars[H >: BothHand, I >: BothHand](
      chromosome: OneHandChromosome[H], otherChromosome: OneHandChromosome[I],
      k1: PhysicalKey[H], charA: OtherChar, charB: OneHandSingleOccupantChar[H],
    ): (OneHandChromosome[H], OneHandChromosome[I]) = {
      val shuffledOtherGenes: List[(PhysicalKey[I], Occupant[I])] = R.shuffle(otherChromosome.toList)
      val (k2, charC, charD) = shuffledOtherGenes.collect {
        case (k2, SingleOccupant(charC: SingleOccupantChar with BothHand, charD)) => (k2, charC, charD)
        case (k2, SingleOccupant(charC, charD: SingleOccupantChar with BothHand)) => (k2, charD, charC)
      } .head
      val newChromosome = chromosome + (k1 -> SingleOccupant[H](charC, charB))
      val newOtherChromosome = otherChromosome + (k2 -> SingleOccupant[I](charA, charD))
      newChromosome -> newOtherChromosome
    }

    val shuffledGenes: List[(PhysicalKey[H], Occupant[H])] = R.shuffle(chromosome.toList)
    val firstChosenGene = shuffledGenes.head
    val (newChromosome, newOtherChromosome) = firstChosenGene match {
      case (k1, o1: DoubleOccupant[H]) =>
        val (k2, o2) = shuffledGenes.filter { case (k2, _) => k2 != k1 } .head
        (chromosome ++ Map(k1 -> o2, k2 -> o1)) -> otherChromosome
      case (k1, o1: SingleOccupant[H]) if R.nextDouble < 0.33 =>
        val (k2, o2) = shuffledGenes.collect { case (k2, o2: SingleOccupant[H]) if k2 != k1 => k2 -> o2 } .head
        (chromosome ++ Map(k1 -> o2, k2 -> o1)) -> otherChromosome
      case (k1, SingleOccupant(charA: OtherChar, charB)) if R.nextDouble < 0.495 =>
        shuffleForOtherChars[H, I](chromosome, otherChromosome, k1, charA, charB)
      case (k1, SingleOccupant(charA, charB: OtherChar)) if R.nextDouble < 0.66 =>
        shuffleForOtherChars[H, I](chromosome, otherChromosome, k1, charB, charA)
      case (k1, SingleOccupant(charA, charB)) =>
        shuffleOneChromosome(chromosome, k1, charA, charB, shuffledGenes) -> otherChromosome
    }
    optimizeChromosome(newChromosome) -> optimizeChromosome(newOtherChromosome)
  }

  def putDoubleOccupantsToChild[H >: BothHand](
    child: TemporalChromosome[H], parent1: OneHandChromosome[H], parent2: OneHandChromosome[H]
  ): OneHandChromosome[H] = {
    def rec(
      keys: List[PhysicalKey[H]], unPutChars: List[DoubleOccupant[H]], putChars: List[DoubleOccupant[H]],
      child: TemporalChromosome[H]
    ): (TemporalChromosome[H], List[DoubleOccupant[H]]) = keys match {
      case Nil => child -> unPutChars
      case k::ks => child.get(k) match {
        case None => rec(ks, unPutChars, putChars, child)
        case Some((Some(_))) => rec(ks, unPutChars, putChars, child)
        case Some(None) =>
          val candidateChar: List[DoubleOccupant[H]] = {
            val parentChars = parent1.get(k) -> parent2.get(k) match {
              case (Some(c1: DoubleOccupant[H]), Some(c2: DoubleOccupant[H])) => List(c1, c2)
              case (Some(c1: DoubleOccupant[H]), _) => List(c1)
              case (_, Some(c2: DoubleOccupant[H])) => List(c2)
              case _ => Nil
            }
            R.shuffle(parentChars).diff(putChars).take(1)
          }
          val updatedChild = child + (k -> candidateChar.headOption)
          val newUnPutChars = R.shuffle(unPutChars.diff(candidateChar))
          val newPutChars = putChars ++ candidateChar
          rec(ks, newUnPutChars, newPutChars, updatedChild)
      }
    }
    val doubleOccupants1: Map[PhysicalKey[H], DoubleOccupant[H]] = parent1.collect {
      case (pk, o: DoubleOccupant[H]) => pk -> o
    }
    val doubleOccupants2: Map[PhysicalKey[H], DoubleOccupant[H]] = parent2.collect {
      case (pk, o: DoubleOccupant[H]) => pk -> o
    }
    val doubleOccupantKeys = R.shuffle((doubleOccupants1.keys ++ doubleOccupants2.keys).toList.distinct)
    val chars = R.shuffle((doubleOccupants1.values ++ doubleOccupants2.values).toList.distinct)
    val (updatedChild, unPutChars) = rec(doubleOccupantKeys, chars, Nil, child)
    val unPutGenes = {
      val genes = updatedChild.toList
      val genesWithDoubleOccupantKeys = genes.filter { case (pk, _) => doubleOccupantKeys.contains(pk) }
      val otherGenes = genes.filterNot { case (pk, _) => doubleOccupantKeys.contains(pk) }
      R.shuffle((genesWithDoubleOccupantKeys ++ otherGenes).collect { case x@(_, None) => x })
    }
    val newChild = updatedChild.map {
      case (pk, Some(o)) => pk -> o
      case (pk, None) => pk -> SingleOccupant[H](EmptyChar, EmptyChar)
    }
    val genesPut = (unPutGenes.zip(unPutChars).map { case ((pk, _), o) => pk -> o }).toMap
    newChild ++ genesPut
  }

  def putSingleOccupantsToChild[H >: BothHand](
    child: OneHandChromosome[H], parent1: OneHandChromosome[H], parent2: OneHandChromosome[H],
    unPutChars: List[SingleOccupantChar with H]
  ): OneHandChromosome[H] = {
    def rec(
      keys: List[PhysicalKey[H]], child: OneHandChromosome[H], putChars: List[SingleOccupantChar with H]
    ): (OneHandChromosome[H], List[SingleOccupantChar with H]) = keys match {
      case Nil => child -> putChars
      case k::ks =>
        val (takeNumber, childChar) = child.get(k) match {
          case Some(SingleOccupant(EmptyChar, EmptyChar)) => 2 -> Some(EmptyChar)
          case Some(SingleOccupant(EmptyChar, c2)) => 1 -> Some(c2)
          case Some(SingleOccupant(c1, EmptyChar)) => 1 -> Some(c1)
          case _ => 0 -> None
        }
        val candidateChars = {
          val parentChars = parent1.get(k) -> parent2.get(k) match {
            case (Some(SingleOccupant(char1, char2)), Some(SingleOccupant(char3, char4))) =>
              List(char1, char2, char3, char4)
            case (Some(SingleOccupant(char1, char2)), _) => List(char1, char2)
            case (_, Some(SingleOccupant(char3, char4))) => List(char3, char4)
            case _ => Nil
          }
          R.shuffle(parentChars).filter(unPutChars.contains(_)).distinct.diff(putChars).take(takeNumber)
        }
        val (chosenChars, updatedChild) = candidateChars -> childChar match {
          case (List(char1, char2), _) => List(char1, char2) -> (child + (k -> SingleOccupant[H](char1, char2)))
          case (List(char1), Some(cc)) => List(char1) -> (child + (k -> SingleOccupant[H](cc, char1)))
          case (Nil, _) => Nil -> child
          case (_, None) => Nil -> child
        }
        val newPutChars = chosenChars ++ putChars
        rec(ks, updatedChild, newPutChars)
    }
    val singleOccupantKeys = R.shuffle(child.toList.collect { case (pk, _: SingleOccupant[H]) => pk })
    val (updatedChild, putChars) = rec(singleOccupantKeys, child, Nil)
    val genes = updatedChild.toList
    val restUnPutChars = unPutChars.diff(putChars)
    def fillEmptyGenes(
      genes: List[(PhysicalKey[H], Occupant[H])], restUnPutChars: List[SingleOccupantChar with H],
      filledGenes: List[(PhysicalKey[H], SingleOccupant[H])] = Nil
    ): List[(PhysicalKey[H], SingleOccupant[H])] = genes -> restUnPutChars match {
      case (Nil, _) => filledGenes
      case (_, Nil) => filledGenes
      case ((pk, SingleOccupant(EmptyChar, EmptyChar))::gs, c1::c2::cs) =>
        fillEmptyGenes(gs, cs, (pk -> SingleOccupant[H](c1, c2))::filledGenes)
      case ((pk, SingleOccupant(EmptyChar, EmptyChar))::gs, c::cs) =>
        fillEmptyGenes(gs, cs, (pk -> SingleOccupant[H](c, EmptyChar))::filledGenes)
      case ((pk, SingleOccupant(EmptyChar, gc))::gs, c::cs) =>
        fillEmptyGenes(gs, cs, (pk -> SingleOccupant[H](gc, c))::filledGenes)
      case ((pk, SingleOccupant(gc, EmptyChar))::gs, c::cs) =>
        fillEmptyGenes(gs, cs, (pk -> SingleOccupant[H](gc, c))::filledGenes)
      case (_::gs, cs) => fillEmptyGenes(gs, cs, filledGenes)
    }
    updatedChild ++ fillEmptyGenes(genes, restUnPutChars).toMap
  }

  def crossover(parent1: LayoutPhoneme, parent2: LayoutPhoneme): LayoutPhoneme = {
    val (leftChromosome1, rightChromosome1) = parent1.leftChromosome -> parent1.rightChromosome
    val (leftChromosome2, rightChromosome2) = parent2.leftChromosome -> parent2.rightChromosome

    val leftTemporalChromosome: TemporalChromosome[LeftHand] = leftKeys.map(_ -> None).toMap
    val rightTemporalChromosome: TemporalChromosome[RightHand] = rightKeys.map(_ -> None).toMap

    val leftDoubleOccupantsPut = putDoubleOccupantsToChild(leftTemporalChromosome, leftChromosome1, leftChromosome2)
    val rightDoubleOccupantsPut = putDoubleOccupantsToChild(rightTemporalChromosome, rightChromosome1, rightChromosome2)

    val leftSingleOccupantsPut =
      putSingleOccupantsToChild(leftDoubleOccupantsPut, leftChromosome1, leftChromosome2, leftSingleOccupantChars)
    val rightSingleOccupantsPut =
      putSingleOccupantsToChild(rightDoubleOccupantsPut, rightChromosome1, rightChromosome2, rightSingleOccupantChars)

    val rightChildEmptyRoom = rightSingleOccupantsPut.map {
      case (_, SingleOccupant(EmptyChar, EmptyChar)) => 2
      case (_, SingleOccupant(EmptyChar, _)) => 1
      case (_, SingleOccupant(_, EmptyChar)) => 1
      case _ => 0
    } .sum
    val rightOtherChars = R.shuffle((rightChromosome1.values ++ rightChromosome2.values).toList.map {
      case SingleOccupant(c1: OtherChar, c2: OtherChar) => List(c1, c2)
      case SingleOccupant(c1: OtherChar, _) => List(c1)
      case SingleOccupant(_, c2: OtherChar) => List(c2)
      case _ => Nil
    } .flatten.distinct).take(rightChildEmptyRoom)
    val leftOtherChars = otherChars.diff(rightOtherChars)

    val leftOthercharsPut =
      putSingleOccupantsToChild(leftSingleOccupantsPut, leftChromosome1, leftChromosome2, leftOtherChars)
    val rightOthercharsPut =
      putSingleOccupantsToChild(rightSingleOccupantsPut, rightChromosome1, rightChromosome2, rightOtherChars)

    LayoutPhoneme(optimizeChromosome(leftOthercharsPut), optimizeChromosome(rightOthercharsPut))
  }

  def evaluateArrangementMemorability(leftChromosome: LeftChromosome, rightChromosome: RightChromosome): Double = {
    val cohesionPenalty =
      evaluatePhonemeCohesion[LeftHand, KRow](leftChromosome) + evaluatePhonemeCohesion[LeftHand, SRow](leftChromosome) +
      evaluatePhonemeCohesion[LeftHand, TRow](leftChromosome) + evaluatePhonemeCohesion[LeftHand, HRow](leftChromosome) +
      evaluatePhonemeCohesion[RightHand, ARow](rightChromosome) + evaluatePhonemeCohesion[RightHand, YRow](rightChromosome) +
      evaluatePhonemeCohesion[RightHand, NRow](rightChromosome) + evaluatePhonemeCohesion[RightHand, MRow](rightChromosome) +
      evaluatePhonemeCohesion[RightHand, RRow](rightChromosome) + evaluatePhonemeCohesion[RightHand, WRow](rightChromosome)
    1.0 + (cohesionPenalty / 100)
  }

  def evaluatePhonemeCohesion[H >: BothHand, R <: RowChar with H : ClassTag](chromosome: OneHandChromosome[H]): Double = {
    val rowCharKeys = chromosome.toList.collect {
      case (pk, SingleOccupant(_: R, _)) => pk
      case (pk, SingleOccupant(_, _: R)) => pk
      case (pk, _: R) => pk
    }
    val numberOfSingleOccupantsHavingTwoSameRowChars = {
      val rowCharNumber = chromosome.collect {
        case (_, SingleOccupant(_: R, _: R)) => 2
        case (_, SingleOccupant(_: R, _)) => 1
        case (_, SingleOccupant(_, _: R)) => 1
        case (_, _: R) => 1
      } .sum
      (rowCharNumber - rowCharKeys.length).toDouble
    }

    val coordinates: List[Coordinate] = rowCharKeys.map(keyCoordinates(_))
    lazy val (stepShapedOrNot, howManyTurned) = {
      val (bool1, howManyTurned1) = keyGroupIsStepShapeOrNot(coordinates)
      val (bool2, howManyTurned2) = keyGroupIsStepShapeOrNot(coordinates.map { case (x, y) => y -> x })
      (bool1 && bool2, howManyTurned1 max howManyTurned2)
    }
    lazy val (shorter, longer) = {
      val (height, width) = getPhonemeHeightAndWidth(coordinates)
      (height min width, height max width)
    }
    val keyGroupDistances = getKeyGroupDistances(coordinates)
    val keyGroupDistancePenalty = (shorter, longer) match {
      case _ if rowCharKeys.length <= 2 => keyGroupDistances.map(d => if (d == getDistance(1.0)) 1.0 else d).sum
      case (1, _) => keyGroupDistances.map(_ => 1.0).sum
      case (2, m) if m <= 4 => keyGroupDistances.map(_ => 1.0).sum
      case (2, _) => keyGroupDistances.map(d => if (d > 2.0) 1.3 else 1.0).sum
      case (3, 3) if rowCharKeys.length >= 4 => keyGroupDistances.map(_ => 1.0).sum
      case _ => (stepShapedOrNot, howManyTurned) match {
        case (true, 0) => keyGroupDistances.map(_ => 1.0).sum
        case (true, 1) => keyGroupDistances.map {
          case 2.0 => 1.0
          case d if d.isWhole && d < 2.0 => 1.0
          case d if d.isWhole && d > 2.0 => 1.3
          case d if d == getDistance(1.0) => 1.0
          case d => d
        } .sum
        case (true, _) => keyGroupDistances.map(d => if (d == getDistance(1.0)) 1.0 else d).sum
        case (false, _) => keyGroupDistances.sum
      }
    }
    keyGroupDistancePenalty + numberOfSingleOccupantsHavingTwoSameRowChars
  }

  def keyGroupIsStepShapeOrNot(coordinates: List[Coordinate]): (Boolean, Int) = {
    val coordinatesOfOneAxis = coordinates.unzip._1.sorted
    val listOfOneAxisCoordinates: List[List[Coordinate]] =
      coordinatesOfOneAxis.map(x => coordinates.filter(_._1 == x).sortBy(_._2))
    def eachCoordinateLinesHigherOrLowerThanAllNextLines(
      listOfOneAxisCoordinates: List[List[Coordinate]], compare: (Int, Int) => Boolean, bools: List[Boolean] = Nil
    ): Boolean = listOfOneAxisCoordinates match {
      case Nil => true
      case _::Nil if bools.nonEmpty => bools.forall(x => x)
      case _::Nil => true
      case cs::css =>
        val coordinates = cs.unzip._2
        val restListOfCoordinates = css.map(_.unzip._2)
        val (lineEndA, lineEndB) = (coordinates.max, coordinates.min)
        val (allRestLineEndsA, allRestlineEndsB) = restListOfCoordinates.map(_.max) -> restListOfCoordinates.map(_.min)
        val endPointsOfRestLinesAreHigherOrlowerThanThisLine =
          allRestLineEndsA.forall(compare(lineEndA, _)) && allRestlineEndsB.forall(compare(lineEndB, _))
        eachCoordinateLinesHigherOrLowerThanAllNextLines(css, compare, endPointsOfRestLinesAreHigherOrlowerThanThisLine::bools)
    }
    val coordinateLinesShapingOneAscendingOrDescendingLine =
      eachCoordinateLinesHigherOrLowerThanAllNextLines(listOfOneAxisCoordinates, _ <= _) ||
      eachCoordinateLinesHigherOrLowerThanAllNextLines(listOfOneAxisCoordinates, _ >= _)
    val howManyLineTurned = listOfOneAxisCoordinates.sliding(2).map {
      case Nil => 0
      case List(_) => 0
      case List(line1, line2) =>
        val (lineEnd1A, lineEnd1B, lineEnd2A, lineEnd2B) = (line1.max, line1.min, line2.max, line2.min)
        if (lineEnd1A == lineEnd2A && lineEnd1B == lineEnd2B) 0 else 1
    } .sum
    coordinateLinesShapingOneAscendingOrDescendingLine -> howManyLineTurned
  }

  def getPhonemeHeightAndWidth(coordinates: List[Coordinate]): (Int, Int) = {
    val (xs, ys) = coordinates.unzip
    (abs(ys.max - ys.min) + 1) -> (abs(xs.max - xs.min) + 1)
  }

  def getKeyGroupDistances(
    coordinates: List[Coordinate], distances: List[Double] = Nil, pastCoordinates: List[Coordinate] = Nil
  ): List[Double] = coordinates match {
    case Nil => distances
    case a::as if pastCoordinates == Nil => getKeyGroupDistances(as, distances, a::pastCoordinates)
    case as => 
      val (minimalDistance, closestKeyToPastCoordinates) = (for ((x1, y1) <- pastCoordinates; (x2, y2) <- as) yield {
        getDistance(x1 - x2, y1 - y2) -> (x2, y2)
      }).minBy(_._1)
      val newCoordinates = as.filter(_ != closestKeyToPastCoordinates)
      getKeyGroupDistances(newCoordinates, minimalDistance::distances, closestKeyToPastCoordinates::pastCoordinates)
  }

  def getDistance(n: Double): Double = getDistance(n, n)
  def getDistance(n: Double, m: Double): Double = sqrt(pow(n, 2) + pow(m, 2))

  def keyCharToCharKey(
    leftChromosome: LeftChromosome, rightChromosome: RightChromosome
  ): CharToKeyMap = {
    val shiftKey: String = leftChromosome.collect { case (PhysicalKey(k), Shift) => k } .head
    val dakutenKey: String = rightChromosome.collect { case (PhysicalKey(k), Dakuten) => k } .head
    val handakutenKey: String = leftChromosome.collect { case (PhysicalKey(k), Handakuten) => k } .head
    def charStringToKeyString(assignableChar: AssignableChar, key: String): List[(String, String)] = assignableChar match {
      case KRow(char) => List(char -> key, voicelessToVoiced(char) -> (key ++ dakutenKey))
      case SRow(char) => List(char -> key, voicelessToVoiced(char) -> (key ++ dakutenKey))
      case TRow(char) => List(char -> key, voicelessToVoiced(char) -> (key ++ dakutenKey))
      case HRow(char) =>
        List(char -> key, voicelessToVoiced(char) -> (key ++ dakutenKey), hToP(char) -> (key ++ handakutenKey))
      case NRow(char) => List(char -> key)
      case MRow(char) => List(char -> key)
      case RRow(char) => List(char -> key)
      case WRow(char) => List(char -> key)
      case ARow(char@"う") =>
        List(char -> key, vowelsToShifted(char) -> (key ++ shiftKey), vu.head -> (key ++ dakutenKey))
      case ARow(char) => List(char -> key, vowelsToShifted(char) -> (key ++ shiftKey))
      case YRow(char) => List(char -> key, vowelsToShifted(char) -> (key ++ shiftKey))
      case OtherChar(char) => List(char -> key)
      case EmptyChar => Nil
    }
    def chromosomeToKeyMap[H >: BothHand](chromosome: OneHandChromosome[H]): Map[String, String] = 
      chromosome.toList .map(_ match {
        case (PhysicalKey(k), SingleOccupant(front, back)) =>
          charStringToKeyString(front, k) ++ charStringToKeyString(back, k ++ shiftKey)
        case (PhysicalKey(k), o: DoubleOccupantChar[H]) => charStringToKeyString(o, k)
        case (_, _: DoubleOccupant[H]) => Nil
      }).flatten.toMap
    val charToKey = chromosomeToKeyMap(leftChromosome) ++ chromosomeToKeyMap(rightChromosome)
    if (charToKey.size != 84) {
      println()
      println(charToKey.size)
      charToKey.toList.sortBy(_._1).foreach(println)
      println(chromosomeToString(leftChromosome, rightChromosome))
      throw new RuntimeException("wrong")
    }
    charToKey
  }

  def getSingleOccupantCharFreqs(): Map[String, Int] = {
    val f = S.fromFile("./src/main/resources/corpus/1gram.csv", "utf-8")
    val lines = (for (l <-f.getLines) yield l.split('\t'))
    val m: Map[String, Int] = (for (l <- lines if allCharacters.contains(l(0))) yield {
      (l(0), l(1).toInt)
    }).toMap
    f.close
    val vowelAndYFreq: Map[String, Int] = vowelsAndYrow.map {
      case c if c == vu.head => c -> (m(c) + m(vowelsToShifted(c)) + m(vu.head))
      case c => c -> (m(c) + m(vowelsToShifted(c)))
    } .toMap
    val voicelessFreq: Map[String, Int] = voiceless.map {
      case c if hRow.contains(c) => c -> (m(c) + m(voicelessToVoiced(c)) + m(hToP(c)))
      case c => c -> (m(c) + m(voicelessToVoiced(c)))
    } .toMap
    val simpleCharsFreq: Map[String, Int] = simpleChars.map(c => c -> m(c)).toMap
    val tenMaruFreq: Map[String, Int] = Map(ten.head -> -1, maru.head -> -2)
    vowelAndYFreq ++ voicelessFreq ++ simpleCharsFreq ++ tenMaruFreq + (EmptyChar.char -> 0)
  }

  def chromosomeToString(t: (LeftChromosome, RightChromosome)): String = chromosomeToString(t._1, t._2)

  def chromosomeToString(leftChromosome: LeftChromosome, rightChromosome: RightChromosome): String = {
    def columnToString[H >: BothHand](column: String, chromosome: OneHandChromosome[H], frontOrBack: Boolean) = {
      column.map(k => chromosome.get(PhysicalKey[H](k.toString)) match {
        case Some(ARow(char)) if !frontOrBack => vowelsToShifted(char)
        case Some(YRow(char)) if !frontOrBack => vowelsToShifted(char)
        case Some(x: DoubleOccupant[H]) => x.char
        case Some(SingleOccupant(frontChar, _)) if frontOrBack => frontChar.char
        case Some(SingleOccupant(_, backChar)) if !frontOrBack => backChar.char
        case None => " *"
      }).mkString
    }
    columnToString[LeftHand]("12345", leftChromosome, true) ++ "  " ++
    columnToString[RightHand]("67890", rightChromosome, true) ++ "\n" ++
    columnToString[LeftHand]("qwert", leftChromosome, true) ++ "  " ++
    columnToString[RightHand]("yuiop", rightChromosome, true) ++ "\n" ++
    columnToString[LeftHand]("asdfg", leftChromosome, true) ++ "  " ++
    columnToString[RightHand]("hjkl;", rightChromosome, true) ++ "\n" ++
    columnToString[LeftHand]("zxcvb", leftChromosome, true) ++ "  " ++
    columnToString[RightHand]("nm,./", rightChromosome, true) ++ "\n" ++
    "----------------------\n" ++
    columnToString[LeftHand]("12345", leftChromosome, false) ++ "  " ++
    columnToString[RightHand]("67890", rightChromosome, false) ++ "\n" ++
    columnToString[LeftHand]("qwert", leftChromosome, false) ++ "  " ++
    columnToString[RightHand]("yuiop", rightChromosome, false) ++ "\n" ++
    columnToString[LeftHand]("asdfg", leftChromosome, false) ++ "  " ++
    columnToString[RightHand]("hjkl;", rightChromosome, false) ++ "\n" ++
    columnToString[LeftHand]("zxcvb", leftChromosome, false) ++ "  " ++
    columnToString[RightHand]("nm,./", rightChromosome, false)
  }
}

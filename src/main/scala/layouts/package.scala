package layout.layouts


package object types {
  type PhysicalKey = String
  type AssignableChar = String

  type ChromosomeJa = Map[PhysicalKey, AssignableChar]
  type ChromosomeJaTuple = Map[PhysicalKey, (AssignableChar, AssignableChar)]

  type CharToKeyMap = Map[AssignableChar, PhysicalKey]
}

package object functions {
  import layout.layouts.types._
  import layout.evaluations.{Evaluator => E}

  def evaluateFitness(charToKey: CharToKeyMap, ngrams: Map[AssignableChar, Int]): Double = {
    var fitness = 0.0
    ngrams.foreach { case (ngram, frequency) =>
      val (keyStrokes, _) = assignableCharsToPhysicalKeys(ngram, charToKey)
      fitness += E.evaluate(keyStrokes) * frequency
    }
    fitness
  }

  def assignableCharsToPhysicalKeys(chars: String, charToKey: CharToKeyMap): (String, String) = {
    var keyStrokes = ""
    var inputChars = ""
    for (assignableChar <- chars if charToKey.get(assignableChar.toString).isDefined) {
      keyStrokes += charToKey(assignableChar.toString)
      inputChars += assignableChar
    }
    keyStrokes -> inputChars
  }
}

package object values {
  import scala.io.{Source => S}
  import layout.layouts.types._

  lazy val twoGrams: Map[AssignableChar, Int] = getTwoGrams
  lazy val threeGrams: Map[AssignableChar, Int] = getThreeGrams
  lazy val fourGrams: Map[AssignableChar, Int] = getFourGrams

  def getTwoGrams(): Map[AssignableChar, Int] = {
    val f = S.fromFile("./src/main/resources/corpus/2gram.csv", "utf-8")
    val lines = (for (l <-f.getLines) yield l.split('\t'))
    val m = (for (l <- lines) yield (l(0), l(1).toInt)).toMap
    f.close
    m
  }

  def getThreeGrams(): Map[AssignableChar, Int] = {
    val f = S.fromFile("./src/main/resources/corpus/3gram.csv", "utf-8")
    val lines = (for (l <-f.getLines) yield l.split('\t'))
    val m = (for (l <- lines) yield (l(0), l(1).toInt)).toMap
    f.close
    m
  }

  def getFourGrams(): Map[AssignableChar, Int] = {
    val f = S.fromFile("./src/main/resources/corpus/4gram.csv", "utf-8")
    val lines = (for (l <-f.getLines) yield l.split('\t'))
    val m = (for (l <- lines) yield (l(0), l(1).toInt)).toMap
    f.close
    m
  }
}

package object implicits {
  import scala.math.Ordering

  implicit object LayoutKeyOrdering extends Ordering[String] {
    val keys = "1234567890qwertyuiop[asdfghjkl;\'zxcvbnm,./".map(_.toString)
    val keysWithNumbers = keys.zip(1 to keys.length).toMap
    def compare(x: String, y: String): Int = keysWithNumbers.get(x) -> keysWithNumbers.get(y) match {
      case _ if x eq y => 0
      case (Some(n), Some(m)) => n compare m
      case (Some(_), None) => -1
      case (None, Some(_)) => 1
      case (None, None) => x compare y
    }
  }
}

package layout.layouts

import scala.util.{Random => R}
import scala.annotation.tailrec

import layout.TypeAlias._
import layout.evaluations.Evaluator


trait LayoutToolsEn extends
LayoutTools with
LeftKeysAndRightKeysEn with
CharsEn with
MakeChromosome with
EvaluateFitness with 
KeyCharToCharKey with
CrossoverChromosomes
{}


trait LayoutTools {
  val leftKeys: List[PhysicalKey]
  val rightKeys: List[PhysicalKey]
  val ngrams: Map[AssignableChar, Int]
  val monograms: Map[AssignableChar, Int]

  def apply(): Layout
  def apply(chromosome: Chromosome): Layout
  def makeChromosome(): Chromosome
  def evaluateFitness(chromosome: Chromosome): Double
  def keyCharToCharKey(chromosome: Chromosome): Map[AssignableChar, PhysicalKey]
  def crossover(p1: Layout, p2: Layout): Layout
  def chromosomeToString(chromosome: Chromosome): String
  def getNgrams(): Map[AssignableChar, Int]
  def getMonograms(): Map[AssignableChar, Int]
}

trait LeftKeysAndRightKeysEn {
  val leftKeys: List[PhysicalKey] = List("q", "w", "e", "r", "t", "a", "s", "d", "f", "g", "z", "x", "c", "v", "b")
  val rightKeys: List[PhysicalKey] = List("y", "u", "i", "o", "p", "h", "j", "k", "l", ";", "n", "m", ",", ".", "/")
  val keys: List[PhysicalKey] = leftKeys ++ rightKeys
}

trait CharsEn {
  val keys: List[PhysicalKey]
  val chars: List[AssignableChar] = keys
}

trait MakeChromosome {
  val keys: List[PhysicalKey]
  val chars: List[AssignableChar]

  def makeChromosome(): Chromosome = {
    val ks = R.shuffle(keys)
    val cs = R.shuffle(chars)

    ks.zip(cs).toMap
  }
}

trait EvaluateFitness extends LayoutTools {
  def evaluateFitness(chromosome: Chromosome): Double = {
    val charToKey = keyCharToCharKey(chromosome)

    (for (ng <- ngrams.keys.toList) yield {
      val typingString = (for (c <- ng) yield {
        charToKey.get(c.toString).getOrElse("")
      }).mkString
      val frequency = ngrams(ng)

      Evaluator.evaluate(typingString) * frequency
    }).sum
  }
}

trait KeyCharToCharKey {
  val keys: List[PhysicalKey]

  def keyCharToCharKey(chromosome: Chromosome): Map[AssignableChar, PhysicalKey] = {
    val chars: List[AssignableChar] = keys.map(chromosome(_))

    chars.zip(keys).toMap
  }
}

trait CrossoverChromosomes {
  def crossoverChromosomes(
    chr1: Chromosome, chr2: Chromosome,
    keys: List[PhysicalKey], chars: List[AssignableChar]
  ): Chromosome = {
    @tailrec
    def f(child: Chromosome, keys: List[PhysicalKey], chars: List[AssignableChar]): Chromosome = {
      val k = keys.head
      val candidateChars: List[AssignableChar] = {
        val parentGenes: List[AssignableChar] = List(chr1(k), chr2(k)).distinct.intersect(chars)

        R.shuffle(parentGenes) ++ chars.diff(parentGenes)
      }
      val newChild = child + (k -> candidateChars.head)

      (keys.tail, candidateChars.tail) match {
        case (Nil, _) => newChild
        case (_, Nil) => newChild
        case (restKeys, restChars) => f(newChild, restKeys, R.shuffle(restChars))
      }
    }

    f(Map[PhysicalKey, AssignableChar](), R.shuffle(keys), R.shuffle(chars))
  }
}

package layout

import scala.util.{Random => R}
import scala.io.{Source => S}

import layout.TypeAlias._


class LayoutSimpleEn(val chromosome: Chromosome, val fitness: Option[Double]) extends Layout {
  import layout.LayoutSimpleEn._

  def mutate(): Layout = {
    val mutationNumber = if (R.nextDouble < 0.7) {
      3
    } else {
      R.shuffle(4 to keys.length).head
    }
    val ks = R.shuffle(keys)
    val chars = ks.map(chromosome(_))
    val (before, after) = ks.splitAt(mutationNumber)
    val mutatedKeys = R.shuffle(before) ++ after
    val newChromosome = mutatedKeys.zip(chars).toMap

    new LayoutSimpleEn(newChromosome, None)
  }

  def evaluate(): Layout = {
    new LayoutSimpleEn(chromosome, Option(evaluateFitness(chromosome)))
  }

  override def toString(): String = {
    chromosomeToString(chromosome) ++ fitness.getOrElse(None).toString
  }
}

object LayoutSimpleEn extends LayoutToolsEn {
  import layout.Evaluator

  val typingPatterns: Map[Chunk, Double] = getTypingPatterns
  val chunkPatterns: Map[Chunk, Double] = typingPatterns.filter(t => !(t._1.length == 2 & t._2 >= 110))
  val nonChunkPatterns: Map[NonChunk, Double] = typingPatterns.filter(t => !chunkPatterns.contains(t._1))
  val ngrams: Map[AssignableChar, Int] = getNgrams
  val evaluator: Evaluator = Evaluator(
    leftKeys,
    rightKeys,
    typingPatterns,
    chunkPatterns,
    nonChunkPatterns
  )


  def apply(): Layout = {
    new LayoutSimpleEn(makeChromosome(), None)
  }

  def apply(chromosome: Chromosome): Layout = {
    new LayoutSimpleEn(chromosome, None)
  }

  def crossover(p1: Layout, p2: Layout): Layout = {
    val newChromosome = crossoverChromosomes(p1.chromosome, p2.chromosome, keys, chars)

    new LayoutSimpleEn(newChromosome, None)
  }

  def getTypingPatterns(): Map[Chunk, Double] = {
    val f = S.fromFile("combinations_data/combinations_made.csv", "utf-8")
    val lines = (for (l <- f.getLines) yield l.split('\t'))
    val m = (for (l <- lines) yield (l(0), l(1).toDouble)).toMap
    f.close

    m
  }

  def getNgrams(): Map[AssignableChar, Int] = {
    val f = S.fromFile("corpus/trigrams.csv", "utf-8")
    val lines = (for (l <-f.getLines) yield l.split('\t'))
    val m = (for (l <- lines) yield (l(0), l(1).toInt)).toMap
    f.close

    m
  }

  def chromosomeToString(chromosome: Chromosome): String = {
    val orderedKeys = "qwertyuiopasdfghjkl;zxcvbnm,./".map(_.toString)

    (for ((k, i) <- orderedKeys.zip(1 to orderedKeys.length)) yield {
      val s = if (i % 10 == 0) {
        "\n"
      } else if (i % 5 == 0) {
        "  "
      } else {
        ""
      }

      chromosome(k).toString ++ s
    }).mkString
  }
}

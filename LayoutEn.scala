package layout

import scala.util.{Random => R}
import scala.io.{Source => S}
import scala.annotation.tailrec

import layout.TypeAlias._


class LayoutEn(val chromosome: Chromosome, val fitness: Option[Double]) extends Layout {
  import layout.LayoutEn._

  def mutate(): Layout = {
    val shuffled = R.shuffle(chromosome).toList
    val mutationNumber = if (R.nextDouble < 0.7) {
      1
    } else {
      R.shuffle(2 to keys.length).head
    }
    val newChromosome = {
      val r = shuffleChromosome(shuffled, mutationNumber).toMap

      if (mutationNumber == 1) r else optimize(r)
    }

    new LayoutEn(newChromosome, None)
  }

  def evaluate(): Layout = {
    new LayoutEn(chromosome, Option(evaluateFitness(chromosome)))
  }

  override def toString(): String = {
    chromosomeToString(chromosome) ++ fitness.getOrElse(None).toString
  }
}

object LayoutEn extends LayoutToolsEn {
  import layout.Evaluator

  val typingPatterns: Map[Chunk, Double] = getTypingPatterns
  val chunkPatterns: Map[Chunk, Double] = typingPatterns.filter(t => !(t._1.length == 2 & t._2 >= 110))
  val chunksAndAlternations: Map[Chunk, Double] = chunkPatterns ++
    (for (l <- leftKeys; r <- rightKeys) yield List(l + r, r + l)).flatten.map(x => x -> 0.0).toMap
  val nonChunkPatterns: Map[NonChunk, Double] = typingPatterns.filter(t => !chunkPatterns.contains(t._1)).map{case (x, y) => x -> 110.0}
  val ngrams: Map[Ngram, Int] = getNgrams
  val bigrams: Map[Ngram, Int] = getBigrams
  val evaluator: Evaluator = Evaluator(
    leftKeys,
    rightKeys,
    typingPatterns,
    chunkPatterns,
    nonChunkPatterns
  )
  val chunkSize: Int = chunksAndAlternations.keys.filter(_.length == 2).map(_.sorted).size
  val sortedBigrams: List[Ngram] = bigrams.toList.sortBy(_._2).map(_._1).filter(x => x.head != x.last)
  val bigramsIndexes: List[Int] = (1 to sortedBigrams.length).map(x => List.fill(x)(x - 1)).toList.flatten

  val ranks: Map[AssignableChar, List[PhysicalKey]] = charsAndOwnBestPlaces
  val monograms: Map[AssignableChar, Int] = getFrequency.toMap


  def apply(): Layout = {
    new LayoutEn(optimize(makeChromosome()), None)
  }

  def apply(chromosome: Chromosome): Layout = {
    new LayoutEn(chromosome, None)
  }

  def crossover(p1: Layout, p2: Layout): Layout = {
    val newChromosome = crossoverChromosomes(p1.chromosome, p2.chromosome, keys, chars)

    new LayoutEn(newChromosome, None)
  }

  @tailrec
  def shuffleChromosome(l: List[(AssignableChar, PhysicalKey)], limit: Int): List[(AssignableChar, PhysicalKey)] = {
    val (geneKey, geneChar) = l.head
    val rest = l.drop(1)
    val rank: List[AssignableChar] = ranks(geneChar)
    val chosenKey: PhysicalKey = R.shuffle(rank.filter(x => x != geneKey)).head
    val chosenGene: (AssignableChar, PhysicalKey) = rest.filter(_._1 == chosenKey).head
    val filteredRest = rest.filter(x => x != chosenGene)
    val newL = filteredRest ++ List((chosenGene._1, geneChar), (geneKey, chosenGene._2))

    if (limit == 1) {
      newL
    } else {
      shuffleChromosome(R.shuffle(newL).toList, limit-1)
    }
  }

  def getKey(chromosome: Chromosome, c: AssignableChar): PhysicalKey = chromosome.find(_._2 == c).get._1

  def optimize(chromosome: Chromosome): Chromosome = {
    def gk = getKey(chromosome, _)

    @tailrec
    def f(chromosome: Chromosome, bigrams: List[Ngram], past: List[Ngram]): Chromosome = {
      def findArpeggioKeyForCharA(
        charA: AssignableChar, charB: AssignableChar,
        keyA: PhysicalKey, keyB: PhysicalKey
      ): List[(PhysicalKey, AssignableChar, PhysicalKey)] = {
        val arpeggioKeysA = keys.filter(x => chunksAndAlternations.isDefinedAt(keyA + x))
        val arpeggioKeysB = keys.filter(x => chunksAndAlternations.isDefinedAt(keyB + x))
        val arpeggioKeys = arpeggioKeysA.intersect(arpeggioKeysB)
        val filtered = arpeggioKeys.filter(x => {
          val c = chromosome(x)
          val relativeBigramKeys = past.filter(y => y.contains(c)).map(y => gk(y.diff(c)))
          val arpeggioKeysX = keys.filter(y => chunksAndAlternations.isDefinedAt(y + x))
          val relativeBigramKeysForCharB = past.filter(_.contains(charB)).map(y => gk(y.diff(charB)))

          relativeBigramKeys.diff(arpeggioKeysB) == Nil &&
          relativeBigramKeysForCharB.diff(arpeggioKeysX) == Nil
        })
        val rank = R.shuffle(ranks(charB).filter(x => filtered.contains(x)))

        rank.map((_, charB, keyB))
      }

      val newChromosome: Chromosome = {
        val b = bigrams.head
        val (c1, c2) = (b.head.toString, b.last.toString)
        val (k1, k2) = (gk(c1), gk(c2))
        val found1 = findArpeggioKeyForCharA(c1, c2, k1, k2)
        val found2 = findArpeggioKeyForCharA(c2, c1, k2, k1)
        val candidates = R.shuffle(List(found1, found2)).flatten

        candidates.map{case (k, oneOfTwoChar, oneOfTwoKey) =>
          val r = chromosome ++ Map(oneOfTwoKey -> chromosome(k), k -> oneOfTwoChar)
          val vs = r.values.toList

          if (vs.length == vs.distinct.length) Some(r) else None
        }.flatten.headOption.getOrElse(chromosome)
      }

      bigrams.tail match {
        case Nil => newChromosome
        case restBigrams => f(newChromosome, restBigrams, bigrams.head +: past)
      }
    }

    val indexes = R.shuffle(bigramsIndexes).distinct.take(chunkSize)
    val takenBigrams = indexes.map(sortedBigrams(_))

    f(chromosome, takenBigrams, Nil)
  }

  def charsAndOwnBestPlaces(): Map[AssignableChar, List[PhysicalKey]] = {
    val chars = getFrequency
    val rankedKeys = keysRank

    (for ((char, cPoint) <- chars) yield {
      val l: List[(PhysicalKey, Int)] = for ((k, kPoint) <- rankedKeys) yield {
        (k, (kPoint - cPoint).abs)
      }
      val maximum = l.maxBy(x => x._2)._2
      val l2: List[(PhysicalKey, Int)] = for ((k, kPoint) <- l) yield {
        (k, (kPoint - maximum).abs + 1)
      }
      val l3: List[PhysicalKey] = (for ((k, kPoint) <- l2) yield {
        List.fill(kPoint)(List(k)).flatten
      }).toList.reduce(_ ++ _)

      (char, l3)
    }).toMap
  }

  def keysRank(): List[(PhysicalKey, Int)] = {
    val tpKeys: List[PhysicalKey] = typingPatterns.keys.toList
    val l: List[(PhysicalKey, Double)] = (for (k <- keys) yield {
      val ks = tpKeys.filter(x => x.contains(k) & x.length == 2)

      (k, ks.map(x => typingPatterns(x)).sum)
    }).sortBy(x => x._2)

    for (((key, _), i) <- l.reverse.zip(1 to keys.length)) yield (key, i)
  }

  def getFrequency(): List[(AssignableChar, Int)] = {
    val ngramKeys = ngrams.keys
    val l: List[(String, Int)] = (for (k <- keys) yield {
      val ngramsOfK: List[String] = ngramKeys.filter(x => x.contains(k)).toList
      val frequencies: Int = ngramsOfK.map(x => ngrams(x)).sum
      val value = ngramsOfK.count(x => x == k) * frequencies

      (k, value)
    }).sortBy(x => x._2)

    for (((char, _), i) <- l.zip((1 to keys.length))) yield (char, i)
  }

  def getTypingPatterns(): Map[Chunk, Double] = {
    val f = S.fromFile("combinations_data/combinations_made.csv", "utf-8")
    val lines = (for (l <-f.getLines) yield l.split('\t'))
    val s = "123457890".toList.map(x => x.toString)
    val m = (for (l <- lines if s.filter(x => l(0).contains(x)).isEmpty) yield (l(0), l(1).toDouble)).toMap
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

  def getBigrams(): Map[AssignableChar, Int] = {
    val f = S.fromFile("corpus/english-bigrams.csv")
    val lines = (for (l <-f.getLines) yield l.split('\t'))
    lines.next
    val ks = (keys.mkString ++ keys.mkString.toUpperCase).distinct
    val tuples = (for (l <- lines if l(0).diff(ks) == "") yield {
      val k = l(0).toLowerCase.replace("'", ";").sorted
      val v = l(1).toInt

      k -> v
    }).toList
    f.close

    val trigramTuples = ngrams.toList.filter(x => x._1.length == 3).map{case (x, y) => (x.head.toString + x.last.toString).sorted -> y}

    (tuples ++ trigramTuples).toList.groupBy(_._1).map{case (x, y) => x -> y.map(_._2).sum}.toMap
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

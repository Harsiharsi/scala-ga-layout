package layout

import scala.util.{Random => R}
import scala.io.{Source => S}
import scala.annotation.tailrec

import layout.TypeAlias._


class LayoutJa(val chromosome: Chromosome, val fitness: Option[Double]) extends Layout {
  import layout.LayoutJa._

  def mutate(): Layout = {
    val mutationNumber = R.nextDouble match {
      case n if n < 0.5 => 1
      case n if n < 0.6 => 2
      case n if n < 0.7 => 3
      case n if n < 0.8 => 4
      case n if n < 0.9 => 5
      case _ => R.shuffle(6 to frontKeys.length / 2).head
    }
    val newChromosome = shuffleChromosome(chromosome, mutationNumber)

    new LayoutJa(newChromosome, None)
  }

  def evaluate(): Layout = {
    new LayoutJa(chromosome, Option(evaluateFitness(chromosome)))
  }

  override def toString(): String = {
    chromosomeToString(chromosome) ++ fitness.getOrElse(None).toString
  }
}

object LayoutJa extends LayoutTools with EvaluateFitness with CrossoverChromosomes {
  import layout.Evaluator

  val frontKeys: List[PhysicalKey] = "123457890qwertyuiopasdfghjkl;zxcvbnm,./".map(_.toString).toList
  val backKeys: List[PhysicalKey] = "!@#$%&*()QWERTYUIOPASDFGHJKL:ZXCVBNM<>?".map(_.toString).toList
  val leftKeys: List[PhysicalKey] = "12345qwertasdfgzxcvb".map(_.toString).toList
  val rightKeys: List[PhysicalKey] = "7890yuiophjkl;nm,./".map(_.toString).toList
  val allKeys: List[PhysicalKey] = frontKeys ++ backKeys
  val frontLeftKeys: List[PhysicalKey] = "12345qwertasdfgzxcvb".map(_.toString).toList
  val frontRightKeys: List[PhysicalKey] = "7890yuiophjkl;nm,./".map(_.toString).toList
  val backLeftKeys: List[PhysicalKey] = "!@#$%QWERTASDFGZXCVB".map(_.toString).toList
  val backRightKeys: List[PhysicalKey] = "&*()YUIOPHJKL:NM<>?L".map(_.toString).toList
  val toFrontKey: Map[PhysicalKey, PhysicalKey] = (backKeys.zip(frontKeys) ++ frontKeys.zip(frontKeys)).toMap
  val toBackKey: Map[PhysicalKey, PhysicalKey] = (backKeys.zip(backKeys) ++ frontKeys.zip(backKeys)).toMap

  val vowelsAndYrow: List[AssignableChar] = "あいうえおゃゅょ".map(_.toString).toList
  val shiftedVowelsAndYrow: List[AssignableChar] = "ぁぃぅぇぉやゆよ".map(_.toString).toList
  val vowelsToShifted: Map[AssignableChar, AssignableChar] = vowelsAndYrow.zip(shiftedVowelsAndYrow).toMap
  val voiceless: List[AssignableChar] = "かきくけこさしすせそたちつてとはひふへほ".map(_.toString).toList
  val voiced: List[AssignableChar] = "がぎぐげござじずぜぞだぢづでどばびぶべぼ".map(_.toString).toList
  val voicelessToVoiced: Map[AssignableChar, AssignableChar] = voiceless.zip(voiced).toMap
  val hRow: List[AssignableChar] = "はひふへほ".map(_.toString).toList
  val pRow: List[AssignableChar] = "ぱぴぷぺぽ".map(_.toString).toList
  val hToP: Map[AssignableChar, AssignableChar] = hRow.zip(pRow).toMap
  val voice: List[AssignableChar] = "なにぬねのまみむめもらりるれろわを".map(_.toString).toList
  val specialMoras: List[AssignableChar] = "っんー".map(_.toString).toList
  val vu: List[AssignableChar] = List("ヴ")
  val ten: List[AssignableChar] = List("、")
  val maru: List[AssignableChar] = List("。")
  val dakuten: List[AssignableChar] = List("゛")
  val handakuten: List[AssignableChar] = List("゜")
  val shift: List[AssignableChar] = List(" S")
  val normalCharacters: List[AssignableChar] = voiceless ++ voice ++ specialMoras ++ ten ++ maru
  val specialCharacters: List[AssignableChar] = vowelsAndYrow ++ dakuten ++ handakuten ++ shift
  val tenAndShiftKey = dakuten ++ handakuten ++ shift
  val specialCharactersToShifted: Map[AssignableChar, AssignableChar] = vowelsToShifted ++ tenAndShiftKey.zip(tenAndShiftKey).toMap
  val shiftedSpecialCharacters: List[AssignableChar] = specialCharactersToShifted.values.toList
  val specialCharactersTuples: List[(AssignableChar, AssignableChar)] = specialCharactersToShifted.toList
  val allCharacters: List[AssignableChar] = vowelsAndYrow ++ shiftedVowelsAndYrow ++ voiceless ++ voiced ++ pRow ++ voice ++ specialMoras ++ vu ++ ten ++ maru
  val allCharsOnLayout: Set[AssignableChar] = (vowelsAndYrow ++ shiftedSpecialCharacters ++ voiceless ++ voice ++ specialMoras ++ ten ++ maru ++ shift ++ dakuten ++ handakuten ++ List("")).toSet

  val typingPatterns: Map[Chunk, Double] = getTypingPatterns
  val chunkPatterns: Map[Chunk, Double] = typingPatterns.filter(t => !(t._1.length == 2 & t._2 >= 110))
  val nonChunkPatterns: Map[NonChunk, Double] = typingPatterns.filter(t => !chunkPatterns.contains(t._1))
  val ngrams: Map[AssignableChar, Int] = getNgrams
  val evaluator: Evaluator = Evaluator(
    leftKeys,
    rightKeys ++ List("'", "["),
    typingPatterns,
    chunkPatterns,
    nonChunkPatterns
  )

  val monograms: Map[AssignableChar, Int] = getMonograms


  def apply(): Layout = {
    new LayoutJa(makeChromosome, None)
  }

  def apply(chromosome: Chromosome): Layout = {
    new LayoutJa(chromosome, None)
  }

  def chromosomeToTupleMode(chromosome: Chromosome): ChromosomeTuple = {
    (for (k <- frontKeys) yield {
      k -> (chromosome(k),chromosome(toBackKey(k)))
    }).toMap
  }

  def chromosomeToLineMode(chromosome: ChromosomeTuple): Chromosome = {
    val frontChromosome = for (k <- frontKeys) yield {
      k -> chromosome(k)._1
    }
    val backChromosome = for (k <- backKeys) yield {
      k -> chromosome(toFrontKey(k))._2
    }

    frontChromosome.toMap ++ backChromosome.toMap
  }

  def makeChromosome(): Chromosome = {
    val putSpecialCharacters: ChromosomeTuple = {
      val nothing = List.fill(allKeys.length)(List("")).flatten
      val zipped = R.shuffle(frontKeys).zip(specialCharacters ++ nothing)

      (for ((k, c) <- zipped) yield {
        c match {
          case "" => k -> (c, c)
          case _ => k -> (c, specialCharactersToShifted(c))
        }
      }).toMap
    }
    val putNormalCharacters: Chromosome = {
      val lineModeChromosome: Chromosome = chromosomeToLineMode(putSpecialCharacters)
      val filteredKeys = lineModeChromosome.keys.filter(x => lineModeChromosome(x) == "").toList
      val shuffled = (R.shuffle(frontKeys) ++ R.shuffle(backKeys)).filter(x => filteredKeys.contains(x))
      val zipped = shuffled.zip(R.shuffle(normalCharacters))

      lineModeChromosome ++ zipped.toMap
    }

    shuffleChromosome(putNormalCharacters, allKeys.length)
  }

  @tailrec
  def shuffleChromosome(chromosome: Chromosome, limit: Int): Chromosome = {
    val k = R.shuffle(allKeys).head
    val geneChar = chromosome(k)
    val specials = specialCharacters ++ shiftedSpecialCharacters
    val newChromosome: Chromosome = if (specials.contains(geneChar)) {
      val tupleMode = chromosomeToTupleMode(chromosome)
      val frontKey = toFrontKey(k)
      val gene: (AssignableChar, AssignableChar) = tupleMode(frontKey)
      val char = gene._1
      val chosenKey: PhysicalKey = toFrontKey(R.shuffle(frontKeys.filter(x => x != frontKey)).head)
      val chosenGene: (AssignableChar, AssignableChar) = tupleMode(chosenKey)
      val newTuples = tupleMode ++ Map(frontKey -> chosenGene, chosenKey -> gene)

      chromosomeToLineMode(newTuples)
    } else {
      val chosenKey: PhysicalKey = {
        val filtered = allKeys.filter(x =>
            x != toFrontKey(k) &&
            x != toBackKey(k) &&
            !specials.contains(chromosome(x)) &&
            geneChar != chromosome(x))

        R.shuffle(filtered).head
      }
      val chosenGene: AssignableChar = chromosome(chosenKey)

      chromosome ++ Map(k -> chosenGene, chosenKey -> geneChar)
    }

    if (limit > 1) {
      shuffleChromosome(newChromosome, limit - 1)
    } else {
      optimize(newChromosome)
    }
  }

  def keyCharToCharKey(chromosome: Chromosome): Map[AssignableChar, PhysicalKey] = {
    val dakutenKey: PhysicalKey = chromosome.keys.filter(x =>
        frontKeys.contains(x) &&
        dakuten.contains(chromosome(x))).head
    val handakutenKey: PhysicalKey = chromosome.keys.filter(x =>
        frontKeys.contains(x) && 
        handakuten.contains(chromosome(x))).head
    val shiftKey: PhysicalKey = chromosome.keys.filter(x =>
        frontKeys.contains(x) &&
        shift.contains(chromosome(x))).head

    val l: List[List[(AssignableChar, PhysicalKey)]] =
      (for (k <- chromosome.keys
              if chromosome(k) != "" &&
                 !(dakuten ++ handakuten ++ shift).contains(chromosome(k)) &&
                 !(backKeys.contains(k) &&
                   specialCharacters.contains(chromosome(k)))) yield {
        val value: AssignableChar = chromosome(k)
        val key: PhysicalKey = frontKeys.contains(k) match {
          case true => k 
          case _ => toFrontKey(k) ++ shiftKey
        }
        val t: List[(AssignableChar, PhysicalKey)] = if (vowelsAndYrow.contains(value)) {
          val r: List[(AssignableChar, PhysicalKey)] = List((value, key), (vowelsToShifted(value), key ++ shiftKey))

          value match {
            case "う" => r :+ (vu.head, key ++ dakutenKey)
            case _ => r
          }
        } else if (voiceless.contains(value)) {
          val r: List[(AssignableChar, PhysicalKey)] = List((value, key), (voicelessToVoiced(value), key ++ dakutenKey))

          hRow.contains(value) match {
            case true => r :+ (hToP(value), key ++ handakutenKey)
            case _ => r
          }
        } else {
          List((value, key))
        }

        t
      }).toList

    l.flatten.toMap
  }

  def crossover(p1: Layout, p2: Layout): Layout = {
    val chr1 = p1.chromosome
    val chr2 = p2.chromosome

    val putSpecialCharacters: Chromosome = {
      val keys = frontKeys.filter(x =>
          frontKeys.contains(x) &&
          (specialCharacters.contains(chr1(x)) || specialCharacters.contains(chr2(x))))
      val child = crossoverChromosomes(chr1, chr2, keys, specialCharacters)
      val shifted = for (k <- child.keys) yield {
        toBackKey(k) -> specialCharactersToShifted(child(k))
      }

      allKeys.map(_ -> "").toMap ++ child ++ shifted
    }
    val putNormalCharacters: Chromosome = {
      val keys = allKeys.filter(x => putSpecialCharacters(x) == "")

      crossoverChromosomes(chr1, chr2, keys, normalCharacters)
    }
    val newChromosome = optimize(putSpecialCharacters ++ putNormalCharacters)

    new LayoutJa(newChromosome, None)
  }

  def optimize(chromosome: Chromosome): Chromosome = {
    val frequnetCharsToFront: Chromosome = {
      val chr: ChromosomeTuple = chromosomeToTupleMode(chromosome)
      val swapped: ChromosomeTuple = 
        for ((k, (c1, c2)) <- chr) yield {
          if (
            specialCharacters.contains(c1) ||
            monograms.get(c1).getOrElse(0) >= monograms.get(c2).getOrElse(0)
          ) {
            k -> (c1, c2)
          } else {
            k -> (c2, c1)
          }
        }

      chromosomeToLineMode(swapped)
    }

    val filled = {
      val genesHaveNoFrontChars: Chromosome =
        frequnetCharsToFront.filter(t => frontKeys.contains(t._1) && t._2 == "")
      val genesHaveBackChars: Chromosome = R.shuffle(
        frequnetCharsToFront.filter(t =>
            backKeys.contains(t._1) &&
            t._2 != "" &&
            !shiftedSpecialCharacters.contains(t._2))).toMap
      val (givenFrontChars, gaveBackChars) =
        (for (((a, b), (c, d)) <- genesHaveNoFrontChars.zip(genesHaveBackChars)) yield {
          ((a -> d), (c -> b))
        }).unzip

      frequnetCharsToFront ++ givenFrontChars.toMap ++ gaveBackChars.toMap
    }

    val backCharsMovedToArpeggioPlacesForShiftKey = {
      val shiftKey = frontKeys.filter(filled(_) == shift.head).head
      val nonArpeggioKeys = frontKeys.
      filter(x => nonChunkPatterns.get(x ++ shiftKey).isDefined).
      map(toBackKey(_))
      val arpeggioKeys = backKeys.filter(x => !nonArpeggioKeys.contains(x) && filled(x) == "")
      val nonArpeggioBackChars = nonArpeggioKeys.
      filter(x =>
          filled(x) != "" &&
          !shiftedSpecialCharacters.contains(filled(x))).
      map(x => x -> filled(x))
      val replaced = (for (((_, char), ak) <- nonArpeggioBackChars.zip(R.shuffle(arpeggioKeys))) yield {
        ak -> char
      }).toMap

      filled ++ nonArpeggioBackChars.map(x => x._1 -> "") ++ replaced
    }

    val backCharsMovedToArpeggioPlacesForDakutenKey = {
      val chars = backCharsMovedToArpeggioPlacesForShiftKey
      val dakutenKey = frontKeys.filter(chars(_) == dakuten.head).head
      val nonArpeggioKeys = frontKeys.
      filter(x => nonChunkPatterns.get(x ++ dakutenKey).isDefined)
      val simpleChars = (voice ++ specialMoras ++ ten ++ maru)
      val arpeggioKeys = frontKeys.filter(x => !nonArpeggioKeys.contains(x) && simpleChars.contains(chars(x))).map(x => x -> chars(x))
      val nonArpeggioBackChars = nonArpeggioKeys.
      filter(x =>
          !simpleChars.contains(chars(x)) &&
          !specialCharacters.contains(chars(x))).
      map(x => x -> chars(x))
      val replaced = (for (((k1, c1), (k2, c2)) <- nonArpeggioBackChars.zip(R.shuffle(arpeggioKeys))) yield {
        List(k1 -> c2, k2 -> c1)
      }).flatten.toMap

      chars ++ replaced
    }

    filled
  }

  def getTypingPatterns(): Map[Chunk, Double] = {
    val f = S.fromFile("combinations_data/combinations_made.csv", "utf-8")
    val lines = (for (l <-f.getLines) yield l.split('\t'))
    val m = (for (l <- lines) yield (l(0), l(1).toDouble)).toMap
    f.close

    m
  }

  def getNgrams(): Map[AssignableChar, Int] = {
    val f = S.fromFile("corpus/4gram.csv", "utf-8")
    val lines = (for (l <-f.getLines) yield l.split('\t'))
    val m = (for (l <- lines) yield (l(0), l(1).toInt)).toMap
    f.close

    m
  }

  def getMonograms(): Map[AssignableChar, Int] = {
    val f = S.fromFile("corpus/1gram.csv", "utf-8")
    val lines = (for (l <-f.getLines) yield l.split('\t'))
    val m: Map[String, Int] = (for (l <- lines if allCharacters.contains(l(0))) yield {
      (l(0), l(1).toInt)
    }).toMap
    f.close

    val vowelAndYFreq: Map[AssignableChar, Int] = (for (c <- vowelsAndYrow) yield {
      val value: Int = m(c) + m(vowelsToShifted(c))

      if (c == vu.head) {
        c -> (value + m(vu.head))
      } else {
        c -> value
      }
    }).toMap
    val voicelessFreq: Map[AssignableChar, Int] = (for (c <- voiceless) yield {
      val value: Int = m(c) + m((voicelessToVoiced(c)))

      if (hRow.contains(c)){
        c -> (value + m(hToP(c)))
      } else {
        c -> value
      }
    }).toMap
    val simpleCharsFreq: Map[AssignableChar, Int] = (for (c <- voice ++ specialMoras) yield {
      c -> m(c)
    }).toMap
    val tenMaruFreq: Map[AssignableChar, Int] = Map(ten.head -> -1, maru.head -> -1)
    val dakutenFreq: Map[AssignableChar, Int] = {
      val frq: Int = (for (c <- vu ++ voiceless) yield {
        if (c == vu.head) {
          m(vu.head)
        } else {
          m(voicelessToVoiced(c))
        }
      }).sum

      Map(dakuten.head -> frq)
    }
    val handakutenFreq: Map[AssignableChar, Int] = {
      val frq: Int = (for (c <- hRow) yield {
        m(hToP(c))
      }).sum

      Map(handakuten.head -> frq)
    }
    val freqs: Map[AssignableChar, Int] = vowelAndYFreq ++ voicelessFreq ++ simpleCharsFreq ++ tenMaruFreq
    val shiftFreq: Map[AssignableChar, Int] = {
      val l = freqs.toList.map(x => x._2).sorted
      val numberOfChars = l.length
      val numberOfFrontKeysForChars = frontKeys.length - (dakuten ++ handakuten ++ shift ++ vowelsAndYrow).length
      val numberOfCharsOnBackKeys = numberOfChars - numberOfFrontKeysForChars
      val frq: Int = l.take(numberOfCharsOnBackKeys).sum + shiftedVowelsAndYrow.map(x => m(x)).sum

      Map(shift.head -> frq)
    }

    freqs ++ dakutenFreq ++ handakutenFreq ++ shiftFreq + ("" -> 0)
  }

  def chromosomeToString(chromosome: Chromosome): String = {
    def f(keys: List[PhysicalKey]): String = {
      (for ((k, i) <- keys.zip(1 to keys.length)) yield {
        val s = if (i % 10 == 0) {
          "\n"
        } else if (i % 5 == 0) {
          "  "
        } else {
          ""
        }
        val s2 = chromosome.get(k) match {
          case None => " *"
          case Some("") => " *"
          case Some(x) => x
        }

        s2 ++ s
      }).mkString
    }
    val orderedFrontKeys = "1234567890qwertyuiopasdfghjkl;zxcvbnm,./".map(_.toString).toList
    val orderedBackKeys = "!@#$%^&*()QWERTYUIOPASDFGHJKL:ZXCVBNM<>?".map(_.toString).toList

    f(orderedFrontKeys) ++ "----------------------\n" ++ f(orderedBackKeys)
  }
}

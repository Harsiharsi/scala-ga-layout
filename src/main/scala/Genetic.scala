package layout

import scala.util.{Random => R}

import layout.layouts.Layout
import layout.layouts.{LayoutJa => L}

import layout.evaluations.Evaluator


object Genetic {

  def main(args: Array[String]): Unit = {
    val POPULATION_SIZE = 300
    val GENERATION_LIMIT = 300

    val targetFitness: Double = {
      import layout.analyzing.LayoutCharToKeyMaps.compositionNo2
      L.evaluateFitness(compositionNo2)
    }

    var populations: List[Layout] = initialization(POPULATION_SIZE)

    var bestLayout: Layout = populations.minBy(_.fitness)
    println("best in the initial populations")
    println(addIndent(bestLayout.toString))
    println(addIndent(s"$targetFitness  target fitness"))

    var nonUpdateCounter: Int = 0
    for (g <- 1 to GENERATION_LIMIT) {
      println()
      if (g % 3 == 0) Evaluator.clearCaches()

      val survivors = {
        val offspring = selection(populations)
        val crossovered = crossover(offspring)
        val mutants = mutation(crossovered)
        evaluation(mutants)
      }

      val bestInTheGeneration = survivors.minBy(_.fitness)
      if (bestInTheGeneration.fitness < bestLayout.fitness) {
        val formerBest = bestLayout
        bestLayout = bestInTheGeneration

        nonUpdateCounter = 0

        println(s"--------------------- generation $g updated ---------------------")
        println("former best")
        println(addIndent(formerBest.toString))
        println("new best")
        println(addIndent(bestLayout.toString))
        val reachedOrNot = if (bestLayout.fitness < targetFitness) " *reached*" else ""
        println(addIndent(s"$targetFitness  target fitness$reachedOrNot"))
      } else {
        nonUpdateCounter += 1

        println(s"generation $g not updated $nonUpdateCounter")
        println("best in the generation")
        println(addIndent(bestInTheGeneration.toString))
      }

      populations = survivors
    }

    println()
    println("result")
    println(bestLayout)
  }

  def select(pops: List[Layout]): List[Layout] = {
    val aspirantSize = 3
    val len = pops.length
    (1 to len).toList.map { _ =>
      val aspirants = for (_ <- 1 to aspirantSize) yield pops(R.nextInt(len))
      aspirants.minBy(_.fitness)
    }
  }

  def initialization(populationSize: Int): List[Layout] = {
    var populations: List[Layout] = (1 to populationSize).toList.map { i =>
      print(s"\r$i")
      L().evaluate
    }
    print("\r")
    populations
  }

  def selection(pops: List[Layout]): List[Layout] = R.shuffle(select(pops))

  def crossover(pops: List[Layout]): List[Layout] = {
    val (before, after) = pops.splitAt(pops.length / 2)
    before.zip(after).map { case (ind1, ind2) =>
      if (R.nextBoolean)
        List(L.crossover(ind1, ind2), L.crossover(ind1, ind2))
      else
        List(ind1, ind2)
    } .flatten
  }

  def mutation(pops: List[Layout]): List[Layout] = {
    val uniques = pops.distinct
    val redundancies = pops.diff(uniques)
    uniques.map(ind => if (R.nextDouble < 0.05) ind.mutate else ind) ++ redundancies.map(_.mutate)
  }

  def evaluation(pops: List[Layout]): List[Layout] = {
    val evaluated = pops.zip(1 to pops.length).map { case (ind, i) =>
      print(s"\r$i")
      if (ind.isEvaluated) ind else ind.evaluate
    }
    print("\r")
    evaluated
  }

  def addIndent(s: String, indent: String = "  ") = s.split("\n").map(indent + _).mkString("\n")
}

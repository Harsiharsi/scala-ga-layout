package layout

import scala.util.{Random => R}

import layout.layouts.Layout
import layout.layouts.{LayoutJa => L}


object Genetic {
  def main(args: Array[String]): Unit = {
    val populationSize = 300
    val generationLimit = 300

    var populations: List[Layout] = (for (i <- 1 to populationSize) yield {
      print(s"\r$i")

      L().evaluate
    }).toList
    println("\r")

    var bestLayout: Layout = populations.minBy(_.fitness.getOrElse(0.0))
    var notUpdated: Int = 0
    println(bestLayout)
    for (g <- 1 to generationLimit) {
      println()

      val offspring = R.shuffle(select(populations))

      val crossovered: List[Layout] = {
        val (before, after) = offspring.splitAt(offspring.length / 2)

        (for ((ind1, ind2) <- before.zip(after)) yield R.nextBoolean match {
          case true =>
            val born1 = L.crossover(ind1, ind2)
            val born2 = L.crossover(ind1, ind2)

            List(born1, born2)
          case _ => List(ind1, ind2)
        }).flatten
      }

      val mutants: List[Layout] = {
        val uniques = crossovered.distinct
        val redundancies = crossovered.diff(uniques)

        (uniques ++ redundancies.map(_.mutate)).take(populationSize)
      }

      val evaluated = for ((ind, i) <- mutants.zip(1 to mutants.length)) yield {
        print(s"\r$i")

        ind.fitness match {
          case None => ind.evaluate
          case _ => ind
        }
      }
      println("\r")

      val bestInTheGeneration = evaluated.minBy(_.fitness.getOrElse(0.0))

      if (
        bestInTheGeneration.fitness.getOrElse(0.0) <
        bestLayout.fitness.getOrElse(0.0)
      ){
        notUpdated = 0
        val formerBest = bestLayout
        bestLayout = bestInTheGeneration

        println(s"--------------------- updated $g ---------------------")
        println(formerBest)
        println()
        println(bestLayout)
      } else {
        notUpdated += 1

        println(s"not updated $g $notUpdated")
      }

      populations = if (notUpdated <= 10) {
        evaluated
      } else {
        for (ind <- evaluated) yield ind == bestLayout match {
          case true => ind.mutate.evaluate
          case _ => ind
        }
      }
    }

    println(bestLayout)
  }

  def select(pops: List[Layout]): List[Layout] = {
    val len = pops.length
    val aspirantSize = 3

    (for (_ <- 1 to len) yield {
      val aspirants = for (_ <- 1 to aspirantSize) yield pops(R.nextInt(len))

      aspirants.minBy(_.fitness.getOrElse(0.0))
    }).toList
  }
}

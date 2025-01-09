package layout.layouts

import layout.layouts.types._


trait Layout {
  type T <: Layout

  private var optionalFitness: Option[Double] = None

  def fitness: Double
  def isEvaluated: Boolean
  def mutate(): T
  def evaluate: T
}

trait LayoutCompanion {
  type T <: Layout

  def crossover(p1: T, p2: T): T
}

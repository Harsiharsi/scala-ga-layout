package layout.layouts

import layout.TypeAlias._


trait Layout {
  val chromosome: Chromosome
  private var optionalFitness: Option[Double] = None

  def fitness: Double
  def isEvaluated: Boolean
  def mutate(): Layout
  def evaluate: Layout
}

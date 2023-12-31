package layout

import layout.TypeAlias._


trait Layout {
  val chromosome: Chromosome
  val fitness: Option[Double]

  def mutate: Layout
  def evaluate: Layout

  override def equals(that: Any): Boolean = that match {
    case that: Layout => chromosome == that.chromosome
    case _ => false
  }
}

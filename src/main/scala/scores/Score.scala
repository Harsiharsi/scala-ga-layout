package layout.scores

import scala.math.Ordered

case class Score(val a: Int, val b: Int, val c: Int, val d: Int, val e: Int) extends Ordered[Score] {
  import scala.math.Ordering.Implicits._

  def +(that: Score): Score =
    Score(a + that.a, b + that.b, c + that.c, d + that.d, e + that.e)
  def +(n: Int): Score = Score(a + n, b + n, c + n, d + n, e + n)
  def *(n: Int): Score = Score(a * n, b * n, c * n, d * n, e * n)
  def toTuple = (a, b, c, d, e)
  override def toString = s"($a $b $c $d $e)"

  override def compare(that: Score): Int =
    if (toTuple == that.toTuple)
      0
    else if (toTuple > that.toTuple)
      1
    else
      -1
}


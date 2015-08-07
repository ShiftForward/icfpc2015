package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model.Command

object Utils {
  def computeScore(sizeOfCurrentUnit: Int, linesClearedByCurrentUnit: Int, linesClearedByLastUnit: Int) = {
    val points = sizeOfCurrentUnit + 100 * (1 + linesClearedByCurrentUnit) * linesClearedByCurrentUnit / 2
    val lineBonus = if (linesClearedByLastUnit > 1) (linesClearedByLastUnit - 1) * points / 10 else 0
    points + lineBonus
  }

  def powerScore(len: Int, reps: Int) = {
    val powerBonus = if (reps > 0) 300 else 0
    2 * len * reps + powerBonus
  }

  def random(rseed: Long) = List(0L).toIterator ++ new Iterator[Long] {
    var seed = rseed
    override def hasNext: Boolean = true
    override def next: Long = { seed = (seed * 1103515245L + 12345L) % 0x100000000L; (seed >> 16) & 0x7FFFL }
  }
}

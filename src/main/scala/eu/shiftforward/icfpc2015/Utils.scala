package eu.shiftforward.icfpc2015

object LinearCongruentialGeneratorTest extends App {
  assert((Utils.random(17) take 11 mkString ", ") == "0, 24107, 16552, 12125, 9427, 13152, 21440, 3383, 6873, 16117, 444")
}

object Utils {
  def computeScore(size: Int, ls: Int, ls_old: Int) = {
    val points = size + 100 * (1 + ls) * ls / 2
    val lineBonus = if (ls_old > 1) (ls_old - 1) * points / 10 else 0
    points + lineBonus
  }

  def random(rseed: Long) = List(0).toIterator ++ new Iterator[Long] {
    var seed = rseed
    override def hasNext: Boolean = true
    override def next: Long = { seed = (seed * 1103515245L + 12345L) % 0x100000000L; (seed >> 16) & 0x7FFFL }
  }
}

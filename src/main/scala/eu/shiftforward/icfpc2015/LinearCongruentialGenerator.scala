package eu.shiftforward.icfpc2015

object LinearCongruentialGeneratorTest extends App {
  assert((LinearCongruentialGenerator.bsdRandom(17) take 11 mkString ", ") == "0, 24107, 16552, 12125, 9427, 13152, 21440, 3383, 6873, 16117, 444")
}

object LinearCongruentialGenerator {
  def bsdRandom(rseed: Long) = List(0).toIterator ++ new Iterator[Long] {
    var seed = rseed
    override def hasNext: Boolean = true
    override def next: Long = { seed = (seed * 1103515245L + 12345L) % 0x100000000L; (seed >> 16) & 0x7FFFL }
  }
}

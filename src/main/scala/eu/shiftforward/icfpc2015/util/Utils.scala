package eu.shiftforward.icfpc2015.util

import scala.collection.immutable.Stream

object Utils {

  def random(rseed: Long) = List(0L).toIterator ++ new Iterator[Long] {
    var seed = rseed
    override def hasNext: Boolean = true
    override def next(): Long = { seed = (seed * 1103515245L + 12345L) % 0x100000000L; (seed >> 16) & 0x7FFFL }
  }

  def insertionSortBy[A, B: Ordering](stream: Stream[A], cost: A => B): Stream[A] = {
    def insertionSortByAux(indexedStream: Stream[(A, Int)]): Stream[(A, Int)] = {
      if (indexedStream.isEmpty) indexedStream
      else {
        lazy val head = indexedStream.minBy { case (x, _) => cost(x) }
        head #:: insertionSortByAux(indexedStream.filterNot { x => x == head })
      }
    }
    insertionSortByAux(stream.zipWithIndex).map(_._1)
  }
}

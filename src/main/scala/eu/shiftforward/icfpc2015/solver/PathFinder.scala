package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015._
import eu.shiftforward.icfpc2015.GridOperations._
import eu.shiftforward.icfpc2015.model._
import scala.collection.mutable

class PathFinder(grid: Grid, from: UnitPos) {
  val commandsToTest = Seq(
    Command('p'),
    Command('b'),
    Command('`'),
    Command('´'),
    Command('d'),
    Command('k'))

  val invertedCommands = Map(
    Command('p') -> Command('b'),
    Command('b') -> Command('p'),
    Command('`') -> Command('l'),
    Command('´') -> Command('a'),
    Command('d') -> Command('k'),
    Command('k') -> Command('d'))

  implicit val ordering = new Ordering[(Int, UnitPos)] {
    def compare(p1: (Int, UnitPos), p2: (Int, UnitPos)) = p1._1 compare p2._1
  }.reverse

  val prev = mutable.Map[UnitPos, (UnitPos, Command, Int)]()

  def pathTo(to: UnitPos): Option[List[Command]] = {
    if (!fits(to, grid))
      None
    else {
      val pq = mutable.PriorityQueue[(Int, UnitPos)]()
      pq.enqueue((0, to))

      def loop() {
        if (!pq.isEmpty) {
          val (_, currentPos) = pq.dequeue
          val dist = if (currentPos == to) 0 else prev(currentPos)._3
          if (currentPos != from) {
            commandsToTest.foreach { command =>
              transform(currentPos, command, grid).foreach { nextPos =>
                if (!prev.contains(nextPos) && nextPos != to) {
                  prev.update(nextPos, (currentPos, command, dist + 1))
                  pq.enqueue((dist + 1 + nextPos.pos.distance(from.pos), nextPos))
                }
              }
            }
            loop()
          }
        }
      }

      loop()

      prev.get(from) match {
        case Some(_) =>
          val c = mutable.ListBuffer[Command]()
          def go(p: UnitPos) {
            if (p != to) {
              prev.get(p) match {
                case Some((pr, comm, _)) =>
                  c += comm
                  go(pr)

                case None => // do nothing
              }
            }
          }

          go(from)
          Some(c.map(invertedCommands).toList)

        case None =>
          None
      }
    }
  }
}

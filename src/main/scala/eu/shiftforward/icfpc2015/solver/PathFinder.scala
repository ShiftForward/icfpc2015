package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015._
import eu.shiftforward.icfpc2015.GridOperations._
import eu.shiftforward.icfpc2015.model._
import scala.collection.mutable

trait PathFindingUtils {
  def commandsToTest: Seq[Command]

  def prev: mutable.Map[UnitPos, (UnitPos, Command, Int)]

  def pathFindingLoop(from: UnitPos, to: UnitPos, grid: Grid) {
    implicit val ordering = new Ordering[(Int, UnitPos)] {
      def compare(p1: (Int, UnitPos), p2: (Int, UnitPos)) = p1._1 compare p2._1
    }.reverse

    val pq = mutable.PriorityQueue[(Int, UnitPos)]()
    pq.enqueue((0, from))

    def loop() {
      if (!pq.isEmpty) {
        val (_, currentPos) = pq.dequeue
        val dist = if (currentPos == from) 0 else prev(currentPos)._3
        if (currentPos != to) {
          commandsToTest.foreach { command =>
            transform(currentPos, command, grid).foreach { nextPos =>
              if (!prev.contains(nextPos) && nextPos != from) {
                prev.update(nextPos, (currentPos, command, dist + 1))
                pq.enqueue((dist + 1 + nextPos.pos.distance(to.pos), nextPos))
              }
            }
          }
          loop()
        }
      }
    }

    loop()
  }

  def buildPath(from: UnitPos, to: UnitPos): List[Command] = {
    val c = mutable.ListBuffer[Command]()
    def go(p: UnitPos) {
      if (p != from) {
        prev.get(p) match {
          case Some((pr, comm, _)) =>
            c += comm
            go(pr)

          case None => // do nothing
        }
      }
    }

    go(to)
    c.toList
  }
}

class PathFinder(grid: Grid, from: UnitPos) extends PathFindingUtils {
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

  val prev = mutable.Map[UnitPos, (UnitPos, Command, Int)]()

  def pathTo(to: UnitPos): Option[List[Command]] = {
    if (!fits(to, grid))
      None
    else {
      pathFindingLoop(to, from, grid)

      prev.get(from) match {
        case Some(_) => Some(buildPath(to, from).map(invertedCommands))
        case None => None
      }
    }
  }
}

class ReversePathFinder(grid: Grid, to: UnitPos) extends PathFindingUtils {
  val commandsToTest = Seq(
    Command('b'),
    Command('p'),
    Command('l'),
    Command('a'),
    Command('k'),
    Command('d'))

  val prev = mutable.Map[UnitPos, (UnitPos, Command, Int)]()

  def pathFrom(from: UnitPos): Option[List[Command]] = {
    if (!fits(to, grid) || !fits(from, grid))
      None
    else {
      pathFindingLoop(from, to, grid)

      prev.get(to) match {
        case Some(_) => Some(buildPath(from, to).reverse)
        case None => None
      }
    }
  }
}

package eu.shiftforward.icfpc2015.solver

import eu.shiftforward.icfpc2015._
import eu.shiftforward.icfpc2015.GridOperations._
import eu.shiftforward.icfpc2015.model._
import scala.collection.mutable

trait PathFindingUtils {
  lazy val commandsToTest = PathFindingUtils.reverseCommandsToTest
  lazy val prev = mutable.Map[UnitPos, (UnitPos, Command, Int)]()

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
        commandsToTest.foreach { command =>
          transform(currentPos, command, grid).foreach { nextPos =>
            if ((!prev.contains(nextPos) || prev(nextPos)._3 > dist + 1) && nextPos != from) {
              prev.update(nextPos, (currentPos, command, dist + 1))
              pq.enqueue((dist + 1 /* + nextPos.pos.distance(to.pos)*/ , nextPos))
            }
          }
        }
        loop()
      }
    }

    if (!prev.contains(from))
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

  def path(from: UnitPos, to: UnitPos, grid: Grid): Option[List[Command]] = {
    if (!fits(to, grid) || !fits(from, grid))
      None
    else if (from == to) Some(List())
    else {
      pathFindingLoop(from, to, grid)

      prev.get(to) match {
        case Some(_) => Some(buildPath(from, to).reverse)
        case None => None
      }
    }
  }
}

object PathFindingUtils {
  final val commandsToTest = Seq(MoveW, MoveE, MoveNW, MoveNE, RotateCW, RotateCCW).map(Command.action)
  final val reverseCommandsToTest = Seq(MoveE, MoveW, MoveSE, MoveSW, RotateCCW, RotateCW).map(Command.action)

  final val invertedCommands = Map(
    MoveW -> MoveE,
    MoveE -> MoveW,
    MoveNW -> MoveSE,
    MoveNE -> MoveSW,
    RotateCW -> RotateCCW,
    RotateCCW -> RotateCW).map { case (k, v) => (Command.action(k), Command.action(v)) }
}

class PathFinder(grid: Grid, from: UnitPos) extends PathFindingUtils {
  def pathTo(to: UnitPos): Option[List[Command]] = path(from, to, grid)
}

class ReversePathFinder(grid: Grid, to: UnitPos) {
  val pathFinder = new PathFinder(grid, to) {
    override lazy val commandsToTest = PathFindingUtils.commandsToTest
  }

  def pathFrom(from: UnitPos): Option[List[Command]] =
    pathFinder.path(to, from, grid).map(_.reverse.map(PathFindingUtils.invertedCommands))
}

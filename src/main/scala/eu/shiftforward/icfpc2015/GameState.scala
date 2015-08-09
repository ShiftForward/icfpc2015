package eu.shiftforward.icfpc2015

import eu.shiftforward.icfpc2015.model._
import GridOperations._
import GameState._

case class GameState(
    grid: Grid,
    units: Seq[CellUnit],
    powerPhrases: Seq[PowerPhrase],
    unitPosState: Option[UnitPosState],
    status: Status,
    score: Option[Score] = Some(Score()),
    commandHistory: Vector[Command] = Vector(),
    placedUnits: Int = 0) {

  def nextState(move: Char): GameState = nextState(Command.char(move))

  def nextState(command: Command): GameState = status match {
    case Failed => this
    case GameOver => copy(status = Failed)
    case Running =>
      val nextCommandHistory = commandHistory :+ command
      lazy val powerPhrasesScored = powerPhrases.filter { phrase => nextCommandHistory.endsWith(phrase.movements) }

      def getNextUnitPos(nextGrid: Grid, nextScore: Option[Score]): GameState = {
        units match {
          case h :: t =>
            initialPosition(h, nextGrid) match {
              case None =>
                GameState(nextGrid, t, powerPhrases, None, GameOver, nextScore, nextCommandHistory, placedUnits + 1)
              case Some(nextPos) =>
                GameState(nextGrid, t, powerPhrases, Some(UnitPosState(nextPos, prevStates = Set(nextPos))), status, nextScore, nextCommandHistory, placedUnits + 1)
            }
          case _ =>
            GameState(nextGrid, units, powerPhrases, None, GameOver, nextScore, nextCommandHistory, placedUnits + 1)
        }
      }

      unitPosState match {
        case Some(prevState @ UnitPosState(pos, _)) =>
          transform(pos, command, grid) match {
            case None =>
              val (nextGrid, removedLines) = removeLines(lockCell(pos, grid))
              val nextScore = score.map(_.updateMove(pos.cells.size, removedLines).updatePower(powerPhrasesScored))
              getNextUnitPos(nextGrid, nextScore)

            case nextPosOpt @ Some(nextPos) =>
              val updatedUnitState = prevState.update(nextPos)
              if (updatedUnitState.valid) {
                val nextScore = score.map(_.updatePower(powerPhrasesScored))
                GameState(grid, units, powerPhrases, Some(updatedUnitState), status, nextScore, nextCommandHistory, placedUnits)
              } else
                GameState(grid, units, powerPhrases, None, Failed, score.map { _ => Score() }, nextCommandHistory, placedUnits)
          }
        case None =>
          println("We shouldn't have gotten here!")
          getNextUnitPos(grid, score)
      }
  }

  def nextState(moves: String): GameState =
    if (moves.isEmpty) this
    else nextState(moves.head).nextState(moves.tail)

  def nextState(moves: Seq[Command]): GameState =
    if (moves.isEmpty) this
    else nextState(moves.head).nextState(moves.tail)

  lazy val currentUnitPos = unitPosState.map(_.unitPos)
}

object GameState {
  sealed trait Status
  case object Running extends Status
  case object GameOver extends Status
  case object Failed extends Status

  case class UnitPosState(unitPos: UnitPos, prevStates: Set[UnitPos] = Set()) {
    lazy val valid = !prevStates.contains(unitPos)

    def update(newUnitPos: UnitPos) =
      UnitPosState(newUnitPos, prevStates + unitPos)
  }

  def apply(grid: Grid, units: Seq[CellUnit], powerPhrases: Seq[PowerPhrase]): GameState =
    apply(grid, units, powerPhrases, countScore = true)

  def apply(grid: Grid, units: Seq[CellUnit], powerPhrases: Seq[PowerPhrase], countScore: Boolean): GameState = {
    val score = if (countScore) Some(Score()) else None

    units match {
      case Nil =>
        GameState(grid, units, powerPhrases, None, GameOver, score)
      case h :: t =>
        initialPosition(h, grid) match {
          case Some(pos) =>
            GameState(grid, t, powerPhrases, Some(UnitPosState(pos, prevStates = Set(pos))), Running, score)
          case None =>
            GameState(grid, t, powerPhrases, None, GameOver, score)
        }
    }
  }
}

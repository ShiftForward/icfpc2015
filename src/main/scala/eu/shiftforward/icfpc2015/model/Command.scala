package eu.shiftforward.icfpc2015.model

trait Command {
  def toChar: Char
}

case class CharCommand(ch: Char) extends Command {
  def toChar = ch
}

sealed trait MoveCommand extends Command

case object MoveW extends MoveCommand {
  def toChar = 'p'
}

case object MoveE extends MoveCommand {
  def toChar = 'b'
}

case object MoveSW extends MoveCommand {
  def toChar = 'a'
}

case object MoveSE extends MoveCommand {
  def toChar = 'l'
}

sealed trait RotateCommand extends Command

case object RotateCW extends RotateCommand {
  def toChar = 'd'
}

case object RotateCCW extends RotateCommand {
  def toChar = 'k'
}

object Command {

  def string(str: String) = str.map(char).toList

  def char(ch: Char) = CharCommand(ch)

  def move(dir: String) = dir match {
    case "W" => MoveW
    case "E" => MoveE
    case "SW" => MoveSW
    case "SE" => MoveSE
  }

  def rotate(dir: String) = dir match {
    case "CW" => RotateCW
    case "CCW" => RotateCCW
  }
}

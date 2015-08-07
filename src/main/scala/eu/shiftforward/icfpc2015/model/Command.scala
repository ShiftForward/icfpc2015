package eu.shiftforward.icfpc2015.model

case class Command(ch: Char) {
  def action = Command.invertedMappings(ch)
}

sealed trait Action
case object MoveW extends Action
case object MoveE extends Action
case object MoveSW extends Action
case object MoveSE extends Action
case object RotateCW extends Action
case object RotateCCW extends Action

object Command {

  def string(str: String) = str.map(char).toList

  def char(ch: Char) = Command(ch.toLower)

  val mappings = Map(
    MoveW -> List('p', ''', '!', '.', '0', '3'),
    MoveE -> List('b', 'c', 'e', 'f', 'y', '2'),
    MoveSW -> List('a', 'g', 'h', 'i', 'j', '4'),
    MoveSE -> List('l', 'm', 'n', 'o', ' ', '5'),
    RotateCW -> List('d', 'q', 'r', 'v', 'z', '1'),
    RotateCCW -> List('k', 's', 't', 'u', 'w', 'x')
  )

  val invertedMappings = mappings.flatMap {
    case (a, chars) =>
      chars.map { ch =>
        ch -> a
      }
  }

  val all = List(MoveW, MoveE, MoveSW, MoveSE, RotateCW, RotateCCW)

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

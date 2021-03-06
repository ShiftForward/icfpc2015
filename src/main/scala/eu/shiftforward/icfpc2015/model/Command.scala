package eu.shiftforward.icfpc2015.model

case class Command(ch: Char) {
  def action = Command.invertedMappings(ch)
  override def toString = ch.toString
}

sealed trait Action
case object MoveW extends Action
case object MoveE extends Action
case object MoveSW extends Action
case object MoveSE extends Action
case object RotateCW extends Action
case object RotateCCW extends Action

// These are used for path finding
case object MoveNW extends Action
case object MoveNE extends Action

object Command {

  def string(str: String) = str.map(char).toList

  def char(ch: Char) = Command(ch.toLower)

  val mappings = Map[Action, List[Char]](
    MoveW -> List('p', ''', '!', '.', '0', '3'),
    MoveE -> List('b', 'c', 'e', 'f', 'y', '2'),
    MoveSW -> List('a', 'g', 'h', 'i', 'j', '4'),
    MoveSE -> List('l', 'm', 'n', 'o', ' ', '5'),
    RotateCW -> List('d', 'q', 'r', 'v', 'z', '1'),
    RotateCCW -> List('k', 's', 't', 'u', 'w', 'x'),
    MoveNW -> List('`'),
    MoveNE -> List('´')
  )

  val invertedMappings = mappings.flatMap {
    case (a, chars) =>
      chars.map { ch =>
        ch -> a
      }
  }

  val all = List(MoveW, MoveE, MoveSW, MoveSE, RotateCW, RotateCCW)

  def action(act: Action) = Command(mappings(act).head)
}

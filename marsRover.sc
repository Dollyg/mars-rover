
import Commands._
import Directions._

sealed trait Direction {
  def left: Direction
  def right: Direction
  def movement: Position
}

object Directions {

  case object North extends Direction {
    override def left: Direction = West
    override def right: Direction = East
    override val movement = Position(0, 1)
  }

  case object East extends Direction {
    override def left: Direction = North
    override def right: Direction = South
    override val movement = Position(1, 0)
  }

  case object West extends Direction {
    override def left: Direction = South
    override def right: Direction = North
    override val movement = Position(-1, 0)
  }

  case object South extends Direction {
    override def left: Direction = East
    override def right: Direction = West
    override val movement = Position(0, -1)
  }

}

sealed trait Command {
  def isInvalid: Boolean = this == Invalid
}

object Command {
  def from(command: String): Command = command match {
    case "L" ⇒ Left
    case "R" ⇒ Right
    case "M" ⇒ Move
    case _   ⇒ Invalid
  }
}

object Commands {
  case object Left extends Command
  case object Right extends Command
  case object Move extends Command
  case object Invalid extends Command
}

case class Position(x: Int, y: Int) {
  def add(other: Position): Position = copy(x + other.x, y + other.y)
}

case class Rover(direction: Direction, position: Position) {
  def move(commands: List[String]): Rover = move2(commands.map(Command.from))

  def move2(commands: List[Command]): Rover =
    commands
      .filterNot(c ⇒ c.isInvalid)
      .foldLeft(this) { case (r, command) ⇒ r.move(command) }


  private def move(command: Command): Rover = command match {
    case Left  ⇒ copy(direction.left)
    case Right ⇒ copy(direction.right)
    case Move  ⇒ copy(position = position.add(direction.movement))
  }
}


//Test Data
Rover(North, Position(1, 2)).move2(List(Left))
Rover(North, Position(1, 2)).move2(List(Right))
Rover(North, Position(1, 2)).move2(List(Move))


Rover(East, Position(1, 2)).move2(List(Left))
Rover(East, Position(1, 2)).move2(List(Right))
Rover(East, Position(1, 2)).move2(List(Move))


val rover = Rover(North, Position(1, 2))
rover.move2(List(Left, Right, Move, Left, Left, Move, Right, Right))
rover.move2(List(Left, Left, Move))

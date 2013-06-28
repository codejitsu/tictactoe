package net.codejitsu.tictactoe

object PlayerType extends Enumeration {
  type PlayerType = Value
  val X, O = Value
}
import PlayerType._

class Player {

}

object Player {
  def apply(name: String, t: PlayerType) = new Player()
}
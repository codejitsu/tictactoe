package net.codejitsu.tictactoe

object PlayerType extends Enumeration {
  type PlayerType = Value
  val X, O = Value
}
import PlayerType._

class Player(val name: String, playerType: PlayerType) {

}

object Player {
  def apply(name: String, playerType: PlayerType) = new Player(name, playerType)
}
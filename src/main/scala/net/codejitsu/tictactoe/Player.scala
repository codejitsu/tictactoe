package net.codejitsu.tictactoe

object PlayerType extends Enumeration {
  type PlayerType = Value
  val X, O = Value
}
import PlayerType._

class Player(val name: String, val playerType: PlayerType, val strategy: PlayStrategy) {
	require(name != null)
	require(!name.trim().isEmpty())
	require(playerType != null)
	require(strategy != null)
	
	def makeMove(field: Field): Move = {
	  strategy.makeMove(field, this)
	}
}

object Player {
  def apply(name: String, playerType: PlayerType, strategy: PlayStrategy) = new Player(name, playerType, strategy)
}
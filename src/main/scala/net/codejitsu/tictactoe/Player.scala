package net.codejitsu.tictactoe

object PlayerType extends Enumeration {
  type PlayerType = Value
  val X, O = Value
}
import PlayerType._

case class Player(val name: String, val playerType: PlayerType, val strategy: PlayStrategy) {
	require(name != null)
	require(!name.trim().isEmpty())
	require(playerType != null)
	require(strategy != null)
	
	def makeMove(board: Board): Move = {
	  strategy.makeMove(board, this)
	}
}
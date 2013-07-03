package net.codejitsu.tictactoe

import net.codejitsu.tictactoe.PlayerType._

object GameStatus extends Enumeration {
  type GameStatus = Value
  val NotStarted, Playing = Value
}

class Game(private val playerX: Player, private val playerO: Player) {
	require(playerX != null && playerO != null)
	require(playerX.name != playerO.name)
	require(playerX.playerType != playerO.playerType)
	
	private var field: Field = Field()
	
	def getPlayer(ptype: PlayerType): Player = if (ptype == PlayerType.X) this.playerX else playerO
	def getField() = field
	def notifyNext(player: PlayerType) = {
	  field = field.update(getPlayer(player).makeMove(getField()))
	}
}

object Game {
  def apply(playerX: Player, playerO: Player) = new Game(playerX, playerO)
}
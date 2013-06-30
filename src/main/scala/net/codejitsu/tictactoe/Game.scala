package net.codejitsu.tictactoe

import net.codejitsu.tictactoe.PlayerType._

class Game(private val playerX: Player, private val playerO: Player) {
	require(playerX != null && playerO != null)
	require(playerX.name != playerO.name)
	require(playerX.playerType != playerO.playerType)
	
	def getPlayer(ptype: PlayerType): Player = if (ptype == PlayerType.X) this.playerX else playerO
}

object Game {
  def apply(playerX: Player, playerO: Player) = new Game(playerX, playerO)
}
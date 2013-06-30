package net.codejitsu.tictactoe

import net.codejitsu.tictactoe.PlayerType._

class Game(private val playerX: Player, private val playerO: Player) {
	def getPlayer(ptype: PlayerType): Player = if (ptype == PlayerType.X) this.playerX else playerO
}

object Game {
  def apply(playerX: Player, playerO: Player) = new Game(playerX, playerO)
}
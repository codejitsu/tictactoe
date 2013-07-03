package net.codejitsu.tictactoe

import net.codejitsu.tictactoe.PlayerType._

object GameStatus extends Enumeration {
  type GameStatus = Value
  val NotStarted, Playing, XWon, OWon, Tie = Value
}

class GameOverException extends Exception

case class Game(val playerX: Player, val playerO: Player) {
	require(playerX != null && playerO != null)
	require(playerX.name != playerO.name)
	require(playerX.playerType != playerO.playerType)
	
	def getPlayer(ptype: PlayerType): Player = if (ptype == PlayerType.X) this.playerX else playerO
	def makeMove(player: PlayerType, field: Field): (Field, Boolean) = {
	  try {
	    (field.update(getPlayer(player).makeMove(field)), true)
	  } catch {
	    case ise: IllegalStateException => {
	     (field, false)
	    }
	  }
	}
}
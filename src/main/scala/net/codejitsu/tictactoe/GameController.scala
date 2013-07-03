package net.codejitsu.tictactoe

import GameStatus._

class GameController(val playerX: Player, val playerO: Player) {
  val game = Game(playerX, playerO)
  
  def gameStatus(): GameStatus = {
    if (game.getField.isEmpty) 
      NotStarted
    else 
      Playing
  }
  
  def start() {
    if (gameStatus() != GameStatus.NotStarted) throw new IllegalStateException
    
    game.notifyNext(nextPlayer())
  }
  
  def getGame() = game
  
  def nextPlayer() = PlayerType.X
}

object GameController {
  def apply(playerX: Player, playerO: Player) = new GameController(playerX, playerO)
}
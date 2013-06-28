package net.codejitsu.tictactoe

class Game(val playerX: Player, val playerO: Player) {

}

object Game {
  def apply(playerX: Player, playerO: Player) = new Game(playerX, playerO)
}
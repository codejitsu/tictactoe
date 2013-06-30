package net.codejitsu.tictactoe.test

import org.junit._
import Assert._

import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType
import net.codejitsu.tictactoe.Game

class GameTest {
  @Test def envTest() {
      assertTrue(true)
  }
  
  @Test def gameHasTwoPlayers() {
    val playerOne = Player("Player 1", PlayerType.X)
    val playerTwo = Player("Player 1", PlayerType.O)
    
    val game = Game(playerOne, playerTwo)
    
    assertNotNull(game.getPlayer(PlayerType.X))
    assertNotNull(game.getPlayer(PlayerType.O))
  }
}
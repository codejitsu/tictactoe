

package net.codejitsu.tictactoe.test

import org.junit.Assert.assertNotNull
import org.junit.Test
import net.codejitsu.tictactoe.Game
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType
import net.codejitsu.tictactoe.PlayStrategy._

class GameTest {
  @Test def gameHasTwoPlayers() {
    val playerOne = Player("Player 1", PlayerType.X, random)
    val playerTwo = Player("Player 2", PlayerType.O, random)
    
    val game = Game(playerOne, playerTwo)
    
    assertNotNull(game.getPlayer(PlayerType.X))
    assertNotNull(game.getPlayer(PlayerType.O))
  }
  
  @Test (expected = classOf[IllegalArgumentException])
  def eachPlayerNameIsUnique() {
    val playerOne = Player("Player 1", PlayerType.X, random)
    val playerTwo = Player("Player 1", PlayerType.O, random)
    
    Game(playerOne, playerTwo)
  }

  @Test (expected = classOf[IllegalArgumentException]) 
  def eachPlayerTypeIsUnique() {
    val playerOne = Player("Player 1", PlayerType.X, random)
    val playerTwo = Player("Player 2", PlayerType.X, random)
    
    Game(playerOne, playerTwo)    
  }
  
  @Test (expected = classOf[IllegalArgumentException]) 
  def playerXIsNull() {
    val playerTwo = Player("Player 2", PlayerType.X, random)
    
    Game(null, playerTwo)    
  }  

  @Test (expected = classOf[IllegalArgumentException]) 
  def playerOIsNull() {
    val playerOne = Player("Player 1", PlayerType.X, random)
    
    Game(playerOne, null)    
  }  

  @Test (expected = classOf[IllegalArgumentException]) 
  def playerXAndOIsNull() {
    Game(null, null)    
  }    
}
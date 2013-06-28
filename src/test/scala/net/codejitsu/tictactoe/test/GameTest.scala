package net.codejitsu.tictactoe.test

package streams

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.codejitsu.tictactoe.Game
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType

@RunWith(classOf[JUnitRunner])
class GameTest extends FunSuite {
  test("test env") {
      assert(true)
  }
  
  test("game has two players") {
    val playerOne = Player("Player 1", PlayerType.X)
    val playerTwo = Player("Player 1", PlayerType.O)
    
    val game = Game(playerOne, playerTwo)
    
    assert(game.playerO != null)
    assert(game.playerX != null)
  }
}
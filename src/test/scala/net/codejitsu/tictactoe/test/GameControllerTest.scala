package net.codejitsu.tictactoe.test

import org.junit.Test
import org.junit.Assert._

import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType
import net.codejitsu.tictactoe.RandomMoveStrategy
import net.codejitsu.tictactoe.GameController
import net.codejitsu.tictactoe.GameStatus

class GameControllerTest {
	@Test def initGameController() {
	  val playerOne = Player("Player 1", PlayerType.X, new RandomMoveStrategy())
      val playerTwo = Player("Player 2", PlayerType.O, new RandomMoveStrategy())
      
      val controller = GameController(playerOne, playerTwo)

      assertEquals(GameStatus.NotStarted, controller.gameStatus())      
      
      controller.start()
      
      assertNotNull(controller.getGame())
      assertEquals(PlayerType.X, controller.nextPlayer())
      assertEquals(GameStatus.Playing, controller.gameStatus())
	}
	
	@Test (expected = classOf[IllegalStateException]) 
	def startCalledTwiceResultsInExceptions() {
	  val playerOne = Player("Player 1", PlayerType.X, new RandomMoveStrategy())
      val playerTwo = Player("Player 2", PlayerType.O, new RandomMoveStrategy())
      
      val controller = GameController(playerOne, playerTwo)

      assertEquals(GameStatus.NotStarted, controller.gameStatus())      
      
      controller.start()
      controller.start()      
	}	
}
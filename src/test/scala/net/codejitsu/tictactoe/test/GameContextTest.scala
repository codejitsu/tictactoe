package net.codejitsu.tictactoe.test

import org.junit.Test
import org.junit.Assert._

import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.RandomMoveStrategy
import net.codejitsu.tictactoe.GameContext
import net.codejitsu.tictactoe.GameStatus._

class GameControllerTest {
	@Test def initGameController() {
	  val playerOne = Player("Player 1", X, new RandomMoveStrategy())
      val playerTwo = Player("Player 2", O, new RandomMoveStrategy())
      
      val context = GameContext(playerOne, playerTwo, X, NotStarted)

      assertEquals(NotStarted, context.status)      
      assertEquals(O, context.nextPlayer)
      
      val afterStart = context.start()
      
      assertNotNull(afterStart._2.game)
      assertEquals(Playing, afterStart._2.status)
	}
	
	@Test (expected = classOf[IllegalStateException]) 
	def startCalledTwiceResultsInExceptions() {
	  val playerOne = Player("Player 1", X, new RandomMoveStrategy())
      val playerTwo = Player("Player 2", O, new RandomMoveStrategy())
      
      val context = GameContext(playerOne, playerTwo, X, NotStarted)

      assertEquals(NotStarted, context.status)      
      
      val afterStart = context.start()
      afterStart._2.start()      
	}
	
	@Test
	def atTheEndTheFieldIsFull() {
	  val playerOne = Player("Player 1", X, new RandomMoveStrategy())
      val playerTwo = Player("Player 2", O, new RandomMoveStrategy())
      
      val context = GameContext(playerOne, playerTwo, X, NotStarted)
      
      val step1 = context.start()
      val step2 = step1._2.move(step1._1, step1._2)
      val step3 = step2._2.move(step2._1, step2._2)
      val step4 = step3._2.move(step3._1, step3._2)
      val step5 = step4._2.move(step4._1, step4._2)
      val step6 = step5._2.move(step5._1, step5._2)
      val step7 = step6._2.move(step6._1, step6._2)
      val step8 = step7._2.move(step7._1, step7._2)
      val step9 = step8._2.move(step8._1, step8._2)      
      
      assertTrue(step9._1.isFull)    
      assertTrue(step9._2.status == XWon || step9._2.status == OWon || step9._2.status == Tie)
	}
	
	@Test
	def playersSwitchedCorrectly() {
	  val playerOne = Player("Player 1", X, new RandomMoveStrategy())
      val playerTwo = Player("Player 2", O, new RandomMoveStrategy())
      
      val context = GameContext(playerOne, playerTwo, X, NotStarted)
      
      assertEquals(X, context.currentPlayer)
      
      val step1 = context.start()
      assertEquals(O, step1._2.currentPlayer)
      
      val step2 = step1._2.move(step1._1, step1._2)
      assertEquals(X, step2._2.currentPlayer)

      val step3 = step2._2.move(step2._1, step2._2)
      assertEquals(O, step3._2.currentPlayer)

      val step4 = step3._2.move(step3._1, step3._2)
      assertEquals(X, step4._2.currentPlayer)

      val step5 = step4._2.move(step4._1, step4._2)
      assertEquals(O, step5._2.currentPlayer)

      val step6 = step5._2.move(step5._1, step5._2)
      assertEquals(X, step6._2.currentPlayer)

      val step7 = step6._2.move(step6._1, step6._2)
      assertEquals(O, step7._2.currentPlayer)

      val step8 = step7._2.move(step7._1, step7._2)
      assertEquals(X, step8._2.currentPlayer)

      val step9 = step8._2.move(step8._1, step8._2)      
      assertEquals(X, step9._2.currentPlayer)
	}	
}
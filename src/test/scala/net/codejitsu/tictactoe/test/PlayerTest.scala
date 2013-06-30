package net.codejitsu.tictactoe.test

import org.junit.Test
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType
import net.codejitsu.tictactoe.RandomMoveStrategy

class PlayerTest {
	@Test (expected = classOf[IllegalArgumentException])
	def nameMustBeNonNull() {
	  Player(null, PlayerType.X, new RandomMoveStrategy())
	}

	@Test (expected = classOf[IllegalArgumentException])
	def nameMustBeNotEmpty() {
	  Player(" ", PlayerType.X, new RandomMoveStrategy())
	}
	
	@Test (expected = classOf[IllegalArgumentException])
	def typeMustBeNotNull() {
	  Player("Player 1", null, new RandomMoveStrategy())
	}	
	
	@Test (expected = classOf[IllegalArgumentException])
	def playStrategyMustBeNotNull() {
	  Player("Player 1", PlayerType.X, null)
	}		
}
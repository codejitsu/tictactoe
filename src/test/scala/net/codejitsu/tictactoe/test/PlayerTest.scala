package net.codejitsu.tictactoe.test

import org.junit.Test
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType
import net.codejitsu.tictactoe.PlayStrategy._

class PlayerTest {
	@Test (expected = classOf[IllegalArgumentException])
	def nameMustBeNonNull() {
	  Player(null, PlayerType.X, random)
	}

	@Test (expected = classOf[IllegalArgumentException])
	def nameMustBeNotEmpty() {
	  Player(" ", PlayerType.X, random)
	}
	
	@Test (expected = classOf[IllegalArgumentException])
	def typeMustBeNotNull() {
	  Player("Player 1", null, random)
	}	
	
	@Test (expected = classOf[IllegalArgumentException])
	def playStrategyMustBeNotNull() {
	  Player("Player 1", PlayerType.X, null)
	}		
}
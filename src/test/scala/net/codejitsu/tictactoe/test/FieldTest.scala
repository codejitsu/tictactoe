package net.codejitsu.tictactoe.test

import org.junit.Test
import org.junit.Assert._
import net.codejitsu.tictactoe.Field
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.Move
import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.RandomMoveStrategy

class FieldTest {
	@Test def initialFieldIsEmpty() {
		val field = Field()
		
		assertTrue(field.isEmpty)
	}
	
	@Test (expected = classOf[IllegalStateException]) 
	def moveOnOccupiedTheSameTypeCellCausesException() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())

		val field = Field()
		val fieldStep2 = field.update(Move(1, 1, player))
		fieldStep2.update(Move(1, 1, player))
	}
	
	@Test (expected = classOf[IllegalStateException]) 
	def moveOnOccupiedTheOtherTypeCellCausesException() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())

		val field = Field()
		val fieldStep2 = field.update(Move(1, 1, player))
		fieldStep2.update(Move(1, 1, Player("Player 1", O, new RandomMoveStrategy())))
	}		
}
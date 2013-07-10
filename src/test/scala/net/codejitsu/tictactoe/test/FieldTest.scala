package net.codejitsu.tictactoe.test

import org.junit.Test
import org.junit.Assert._
import net.codejitsu.tictactoe.Field
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.Move
import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.CellStatus._
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
	
	@Test def testFullRow() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val field = Field()
	  	
	  	val field1 = field.update(Move(0, 0, player))
	  	val field2 = field1.update(Move(0, 1, player))	  	
	    val field3 = field2.update(Move(0, 2, player))
	    
	    assertEquals(Set((OccupiedByX, 0, 0), (OccupiedByX, 0, 1), (OccupiedByX, 0, 2)), field3.getRow(0))
	}
	
	@Test def testFullColumn() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val field = Field()
	  	
	  	val field1 = field.update(Move(0, 1, player))
	  	val field2 = field1.update(Move(1, 1, player))	  	
	    val field3 = field2.update(Move(2, 1, player))
	    
	    assertEquals(Set((OccupiedByX, 0, 1), (OccupiedByX, 1, 1), (OccupiedByX, 2, 1)), field3.getColumn(1))
	}	

	@Test def testFirstDiagonal() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val field = Field()
	  	
	  	val field1 = field.update(Move(0, 0, player))
	  	val field2 = field1.update(Move(1, 1, player))	  	
	    val field3 = field2.update(Move(2, 2, player))
	    
	    assertEquals(Set((OccupiedByX, 0, 0), (OccupiedByX, 1, 1), (OccupiedByX, 2, 2)), field3.getFirstDiagonal())
	}	

	@Test def testSecondDiagonal() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val field = Field()
	  	
	  	val field1 = field.update(Move(0, 2, player))
	  	val field2 = field1.update(Move(1, 1, player))	  	
	    val field3 = field2.update(Move(2, 0, player))
	    
	    assertEquals(Set((OccupiedByX, 0, 2), (OccupiedByX, 1, 1), (OccupiedByX, 2, 0)), field3.getSecondDiagonal())
	}		
	
	@Test (expected = classOf[IllegalArgumentException]) 
	def testInvalidRow() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val field = Field()

	  	field.getRow(3)
	}

	@Test (expected = classOf[IllegalArgumentException]) 
	def testNegativeRow() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val field = Field()

	  	field.getRow(-1)
	}
	
	@Test (expected = classOf[IllegalArgumentException]) 
	def testInvalidColumn() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val field = Field()

	  	field.getColumn(3)
	}

	@Test (expected = classOf[IllegalArgumentException]) 
	def testNegativeColumn() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val field = Field()

	  	field.getColumn(-1)
	}	
}
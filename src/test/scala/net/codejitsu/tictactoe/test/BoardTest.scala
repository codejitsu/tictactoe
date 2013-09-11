package net.codejitsu.tictactoe.test

import org.junit.Test
import org.junit.Assert._
import net.codejitsu.tictactoe.Board
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.Move
import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.CellStatus._
import net.codejitsu.tictactoe.RandomMoveStrategy
import net.codejitsu.tictactoe.Cell

class FieldTest {
	@Test def initialFieldIsEmpty() {
		val board = Board()
		
		assertTrue(board.isEmpty)
	}
	
	@Test (expected = classOf[IllegalStateException]) 
	def moveOnOccupiedTheSameTypeCellCausesException() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())

		val board = Board()
		val boardStep2 = board.update(Move(Cell(1, 1), player))
		boardStep2.update(Move(Cell(1, 1), player))
	}
	
	@Test (expected = classOf[IllegalStateException]) 
	def moveOnOccupiedTheOtherTypeCellCausesException() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())

		val board = Board()
		val boardStep2 = board.update(Move(Cell(1, 1), player))
		boardStep2.update(Move(Cell(1, 1), Player("Player 1", O, new RandomMoveStrategy())))
	}	
	
	@Test def testFullRow() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val board = Board()
	  	
	  	val board1 = board.update(Move(Cell(0, 0), player))
	  	val board2 = board1.update(Move(Cell(0, 1), player))	  	
	    val board3 = board2.update(Move(Cell(0, 2), player))
	    
	    assertEquals(Set(Cell(0, 0, OccupiedByX), Cell(0, 1, OccupiedByX), Cell(0, 2, OccupiedByX)), 
	        board3.getRow(0))
	}
	
	@Test def testFullColumn() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val board = Board()
	  	
	  	val board1 = board.update(Move(Cell(0, 1), player))
	  	val board2 = board1.update(Move(Cell(1, 1), player))	  	
	    val board3 = board2.update(Move(Cell(2, 1), player))
	    
	    assertEquals(Set(Cell(0, 1, OccupiedByX), Cell(1, 1, OccupiedByX), Cell(2, 1, OccupiedByX)), 
	        board3.getColumn(1))
	}	

	@Test def testFirstDiagonal() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val board = Board()
	  	
	  	val board1 = board.update(Move(Cell(0, 0), player))
	  	val board2 = board1.update(Move(Cell(1, 1), player))	  	
	    val board3 = board2.update(Move(Cell(2, 2), player))
	    
	    assertEquals(Set(Cell(0, 0, OccupiedByX), Cell(1, 1, OccupiedByX), Cell(2, 2, OccupiedByX)), 
	        board3.getFirstDiagonal())
	}	

	@Test def testSecondDiagonal() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val board = Board()
	  	
	  	val board1 = board.update(Move(Cell(0, 2), player))
	  	val board2 = board1.update(Move(Cell(1, 1), player))	  	
	    val board3 = board2.update(Move(Cell(2, 0), player))
	    
	    assertEquals(Set(Cell(0, 2, OccupiedByX), Cell(1, 1, OccupiedByX), Cell(2, 0, OccupiedByX)), 
	        board3.getSecondDiagonal())
	}		
	
	@Test (expected = classOf[IllegalArgumentException]) 
	def testInvalidRow() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val board = Board()

	  	board.getRow(3)
	}

	@Test (expected = classOf[IllegalArgumentException]) 
	def testNegativeRow() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val board = Board()

	  	board.getRow(-1)
	}
	
	@Test (expected = classOf[IllegalArgumentException]) 
	def testInvalidColumn() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val board = Board()

	  	board.getColumn(3)
	}

	@Test (expected = classOf[IllegalArgumentException]) 
	def testNegativeColumn() {
	  	val player = Player("Player 1", X, new RandomMoveStrategy())
	  	
	  	val board = Board()

	  	board.getColumn(-1)
	}	
}
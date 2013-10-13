package net.codejitsu.tictactoe.test

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

import net.codejitsu.tictactoe.Board
import net.codejitsu.tictactoe.Cell
import net.codejitsu.tictactoe.CellStatus.OccupiedByX
import net.codejitsu.tictactoe.Move
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType.O
import net.codejitsu.tictactoe.PlayerType.X
import net.codejitsu.tictactoe.PlayStrategy._

class BoardTest {
	@Test def initialFieldIsEmpty() {
		val board = Board()
		
		assertTrue(board.isEmpty)
	}
	
	@Test
	def moveOnOccupiedTheSameTypeCellCausesException() {
		val board = Board()
		val boardStep2 = board.update(Move(Cell(1, 1), X))
		assertEquals(None, boardStep2.get.update(Move(Cell(1, 1), X)))
	}
	
	@Test
	def moveOnOccupiedTheOtherTypeCellCausesException() {
		val board = Board()
		val boardStep2 = board.update(Move(Cell(1, 1), X))
		assertEquals(None, boardStep2.get.update(Move(Cell(1, 1), O)))
	}	
	
	@Test def testFullRow() {
	  	val board = Board()
	  	
	  	val board1 = board.update(Move(Cell(0, 0), X))
	  	val board2 = board1.get.update(Move(Cell(0, 1), X))	  	
	    val board3 = board2.get.update(Move(Cell(0, 2), X))
	    
	    assertEquals(Set(Cell(0, 0, OccupiedByX), Cell(0, 1, OccupiedByX), Cell(0, 2, OccupiedByX)), 
	        board3.get.getRow(0))
	}
	
	@Test def testFullColumn() {
	  	val board = Board()
	  	
	  	val board1 = board.update(Move(Cell(0, 1), X))
	  	val board2 = board1.get.update(Move(Cell(1, 1), X))	  	
	    val board3 = board2.get.update(Move(Cell(2, 1), X))
	    
	    assertEquals(Set(Cell(0, 1, OccupiedByX), Cell(1, 1, OccupiedByX), Cell(2, 1, OccupiedByX)), 
	        board3.get.getColumn(1))
	}	

	@Test def testFirstDiagonal() {
	  	val board = Board()
	  	
	  	val board1 = board.update(Move(Cell(0, 0), X))
	  	val board2 = board1.get.update(Move(Cell(1, 1), X))	  	
	    val board3 = board2.get.update(Move(Cell(2, 2), X))
	    
	    assertEquals(Set(Cell(0, 0, OccupiedByX), Cell(1, 1, OccupiedByX), Cell(2, 2, OccupiedByX)), 
	        board3.get.getFirstDiagonal())
	}	

	@Test def testSecondDiagonal() {
	  	val board = Board()
	  	
	  	val board1 = board.update(Move(Cell(0, 2), X))
	  	val board2 = board1.get.update(Move(Cell(1, 1), X))	  	
	    val board3 = board2.get.update(Move(Cell(2, 0), X))
	    
	    assertEquals(Set(Cell(0, 2, OccupiedByX), Cell(1, 1, OccupiedByX), Cell(2, 0, OccupiedByX)), 
	        board3.get.getSecondDiagonal())
	}		
	
	@Test (expected = classOf[IllegalArgumentException]) 
	def testInvalidRow() {
	  	val player = Player("Player 1", X, random)
	  	
	  	val board = Board()

	  	board.getRow(3)
	}

	@Test (expected = classOf[IllegalArgumentException]) 
	def testNegativeRow() {
	  	val player = Player("Player 1", X, random)
	  	
	  	val board = Board()

	  	board.getRow(-1)
	}
	
	@Test (expected = classOf[IllegalArgumentException]) 
	def testInvalidColumn() {
	  	val player = Player("Player 1", X, random)
	  	
	  	val board = Board()

	  	board.getColumn(3)
	}

	@Test (expected = classOf[IllegalArgumentException]) 
	def testNegativeColumn() {
	  	val player = Player("Player 1", X, random)
	  	
	  	val board = Board()

	  	board.getColumn(-1)
	}	
}
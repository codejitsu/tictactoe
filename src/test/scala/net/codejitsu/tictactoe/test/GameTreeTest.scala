package net.codejitsu.tictactoe.test

import org.junit.Test
import org.junit.Assert._
import net.codejitsu.tictactoe.GameTree
import net.codejitsu.tictactoe.GameTree
import net.codejitsu.tictactoe.Field
import net.codejitsu.tictactoe.Leaf
import net.codejitsu.tictactoe.Node
import net.codejitsu.tictactoe._

class GameTreeTest {
	@Test
	def zeroLevelIsEmpty() {
	  val zeroLevel = GameTree.start
	  
	  zeroLevel match {
	    case Leaf => fail()
	    case Node(e, ch, p) => assertTrue(ch.isEmpty)
	  }
	}
	
	@Test
	def zeroElementIsEmptyField() {
	  val zeroLevel = GameTree.start
	  
	  zeroLevel match {
	    case Leaf => fail()
	    case Node(e, ch, p) => e match {
	      case Some(x) => assertTrue(x.isEmpty)
	      case _ => fail()
	    }
	  }
	}

	@Test
	def zeroElementHasXasNextPlayer() {
	  val zeroLevel = GameTree.start
	  
	  zeroLevel match {
	    case Leaf => fail()
	    case Node(e, ch, p) => assertTrue(p == PlayerType.X)
	  }
	}	
	
	@Test
	def firstLevelIsNotEmptyConsistsOfNineElements() {
	  val firstLevel = GameTree.build(GameTree.start, 1)
	  
	  firstLevel match {
	    case Leaf => fail()
	    case Node(e, ch, p) => {
	      assertTrue(!ch.isEmpty)
	      assertEquals(FieldSize * FieldSize, ch.size)
	    }
	  }
	}	
}
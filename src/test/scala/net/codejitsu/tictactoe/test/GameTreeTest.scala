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
	  val firstLevel = GameTree.build(GameTree.start, 1, 2)
	  
	  firstLevel match {
	    case Leaf => fail()
	    case Node(e, ch, p) => {
	      assertTrue(!ch.isEmpty)
	      assertEquals(FieldSize * FieldSize, ch.size)
	    }
	  }
	}
	
	@Test
	def firstLevelAllElementsAreNodes() {
	  val firstLevel = GameTree.build(GameTree.start, 1, 2)
	  
	  firstLevel match {
	    case Leaf => fail()
	    case Node(e, ch, p) => {
	    	ch.foreach(c => c match {
	    	  case Leaf => fail()
	    	  case _ => ()
	    	})
	    }
	  }
	}	
	
	@Test
	def firstLevelContainsAllCorrectMoves() {
	  val firstLevel = GameTree.build(GameTree.start, 1, 2)
	  
	  val fieldInit = Field()
	  val player = Player("Player", PlayerType.X, new RandomMoveStrategy)
	  val moves =  List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))
	  
	  val allFields = moves.map(m => fieldInit.update(Move(m._1, m._2, player))).map(_.toString)
	  
	  firstLevel match {
	    case Leaf => fail()
	    case Node(e, ch, p) => {
	    	ch.foreach(c => c match {
	    	  case Leaf => fail()
	    	  case x => allFields.contains(x.toString)
	    	})
	    }
	  }	  
	}
	
	@Test
	def eachNodeOnSecondLevelHasEightChildren() {
	  val firstLevel = GameTree.build(GameTree.start, 1, 2)
	  val secondLevel = GameTree.build(firstLevel, 1, 3)

	  secondLevel match {
	    case Leaf => fail()
	    case Node(e, ch, p) => {
	    	ch.foreach(c => c match {
	    	  case Leaf => fail()
	    	  case Node(e2, ch2, p2) => {
	    		  assertTrue(!ch2.isEmpty)
	    		  assertEquals(FieldSize * FieldSize - 1, ch2.size)
	    	  }
	    	})	      
	    }
	  }
	}
	
	@Test
	def generateCompleteTree() {
	  val tree = buildTree(GameTree.start, 1, 9)
	 // GameTree.printLevel(tree, 0, 2)
	}
	
	def buildTree(tree: GameTree[Field], level: Int, upToLevel: Int): GameTree[Field] = {
	  if (level == upToLevel) tree
	  else {
	    buildTree(GameTree.build(tree, 1, level + 1), level + 1, upToLevel)
	  }
	}
}
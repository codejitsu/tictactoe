package net.codejitsu.tictactoe.test

import org.junit.Test
import org.junit.Assert._
import net.codejitsu.tictactoe.GameTree
import net.codejitsu.tictactoe.GameTree
import net.codejitsu.tictactoe.Field
import net.codejitsu.tictactoe.Leaf
import net.codejitsu.tictactoe.Node
import net.codejitsu.tictactoe._
import net.codejitsu.tictactoe.PlayerType._

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
	    case Node(e, _, _) => assertTrue(e.isEmpty)
	  }
	}

	@Test
	def zeroElementHasOasNextPlayer() {
	  val zeroLevel = GameTree.start
	  
	  zeroLevel match {
	    case Leaf => fail()
	    case Node(e, ch, p) => assertTrue(p == PlayerType.O)
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
	def secondLevelContainsAllCorrectMoves() {
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
	    		  
	    		  val allMoves = generateAllMoves2Level(e2).map(_.toString)
	    		  
	    		  ch2.forall(
	    		    c => allMoves.contains(c.toString) 
	    		  )
	    	  }
	    	})	      
	    }
	  }
	}
	
	@Test
	def generateCompleteTree() {
	  val tree = buildTree(GameTree.start, 1, 9)
	}
	
	@Test
	def testAdviceXWonOrTie() {
	  val tree = buildTree(GameTree.start, 1, 9)
	  val moves = generateWinPath(tree, PlayerType.X)
	  
	  assertTrue(moves.moves.size > 5)
	  
	  val game = Game(Player("X", PlayerType.X, new RandomMoveStrategy()), 
	      Player("O", PlayerType.O, new RandomMoveStrategy()))
	  
	  val status = game.calculateStatus(moves.moves.last)

	  assertTrue(status == GameStatus.XWon || status == GameStatus.Tie)
	}

	@Test
	def testAdviceOWonOrTie() {
	  val tree = buildTree(GameTree.start, 1, 9)
	  val moves = generateWinPath(tree, PlayerType.O)
	  
	  assertTrue(moves.moves.size > 5)

	  val game = Game(Player("X", PlayerType.X, new RandomMoveStrategy()), 
	      Player("O", PlayerType.O, new RandomMoveStrategy()))
	  
	  val status = game.calculateStatus(moves.moves.last)

	  assertTrue(status == GameStatus.OWon || status == GameStatus.Tie)
	}	
	
	def generateWinPath(start: GameTree, playerToWin: PlayerType): MovePath = {
	  GameTree.advice(start, playerToWin)
	}
	
	def generateAllMoves2Level(field: Field): List[Field] = {
	  val xmove = field.field(0)
	  
	  val player = Player("Player", PlayerType.O, new RandomMoveStrategy)
	  val moves =  List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))
	  
	  moves.filter(m => m._1 != xmove._2 && m._2 != xmove._3).map(m => field.update(Move(m._1, m._2, player)))	  
	}
	
	def buildTree(tree: GameTree, level: Int, upToLevel: Int): GameTree = {
	  if (level == upToLevel) tree
	  else {
	    buildTree(GameTree.build(tree, 1, level + 1), level + 1, upToLevel)
	  }
	}
}
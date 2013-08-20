package net.codejitsu.tictactoe.test

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test
import net.codejitsu.tictactoe.Field
import net.codejitsu.tictactoe.FieldSize
import net.codejitsu.tictactoe.Game
import net.codejitsu.tictactoe.GameStatus._
import net.codejitsu.tictactoe.GameTree
import net.codejitsu.tictactoe.Leaf
import net.codejitsu.tictactoe.Move
import net.codejitsu.tictactoe.MovePath
import net.codejitsu.tictactoe.Node
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType.O
import net.codejitsu.tictactoe.PlayerType.PlayerType
import net.codejitsu.tictactoe.PlayerType.X
import net.codejitsu.tictactoe.RandomMoveStrategy
import org.junit.Ignore

class GameTreeTest {
	@Test
	def zeroLevelIsEmpty() {
	  val zeroLevel = GameTree.start
	  
	  zeroLevel match {
	    case Node(e, ch, p, _) => assertTrue(ch.isEmpty)
	    case _ => fail()
	  }
	}
	
	@Test
	def zeroElementIsEmptyField() {
	  val zeroLevel = GameTree.start
	  
	  zeroLevel match {
	    case Node(e, _, _, _) => assertTrue(e.isEmpty)
	    case _ => fail()
	  }
	}

	@Test
	def zeroElementHasXasNextPlayer() {
	  val zeroLevel = GameTree.start
	  
	  zeroLevel match {
	    case Node(_, _, p, _) => assertTrue(p == X)
	    case _ => fail()
	  }
	}	
	
	@Test
	def firstLevelIsNotEmptyConsistsOfNineElements() {
	  val firstLevel = GameTree.build(GameTree.start, 1)
	  
	  firstLevel match {
	    case Leaf(_) => fail()
	    case Node(e, ch, p, _) => {
	      assertTrue(!ch.isEmpty)
	      assertEquals(FieldSize * FieldSize, ch.size)
	    }
	  }
	}
	
	@Test
	def firstLevelAllElementsAreNodes() {
	  val firstLevel = GameTree.build(GameTree.start, 1)
	  
	  firstLevel match {
	    case Leaf(_) => fail()
	    case Node(e, ch, p, _) => {
	    	ch.foreach(c => c match {
	    	  case Leaf(_) => fail()
	    	  case _ => ()
	    	})
	    }
	  }
	}	
	
	@Test
	def firstLevelContainsAllCorrectMoves() {
	  val firstLevel = GameTree.build(GameTree.start, 1)
	  
	  val fieldInit = Field()
	  val player = Player("Player", X, new RandomMoveStrategy)
	  val moves =  List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))
	  
	  val allFields = moves.map(m => fieldInit.update(Move(m._1, m._2, player))).map(_.toString)
	  
	  firstLevel match {
	    case Leaf(_) => fail()
	    case Node(e, ch, p, _) => {
	    	ch.foreach(c => c match {
	    	  case Leaf(_) => fail()
	    	  case x => allFields.contains(x.toString)
	    	})
	    	
	    	assertTrue(allFields.size == ch.size)
	    }
	  }	  
	}
	
	@Test
	def eachNodeOnSecondLevelHasEightChildren() {
	  val secondLevel = GameTree.build(GameTree.start, 1)

	  secondLevel match {
	    case Leaf(_) => fail()
	    case Node(e, ch, p, _) => {
	    	ch.foreach(c => c match {
	    	  case Leaf(_) => fail()
	    	  case Node(e2, ch2, p2, _) => {
	    		  assertTrue(!ch2.isEmpty)
	    		  assertEquals(FieldSize * FieldSize - 1, ch2.size)
	    	  }
	    	})	      
	    }
	  }
	}

	@Test
	def secondLevelContainsAllCorrectMoves() {
	  val firstLevel = GameTree.build(GameTree.start, 1)

	  firstLevel match {
	    case Leaf(_) => fail()
	    case Node(e, ch, p, _) => {
	    	ch.foreach(c => c match {
	    	  case Leaf(_) => fail()
	    	  case Node(e2, ch2, p2, _) => {
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
	  val tree = buildTree(GameTree.start, 1)
	}
	
	@Test
	@Ignore
	def testAdviceXWonOrTie() {
	  val tree = buildTree(GameTree.start, 1)
	  val path = generateWinPath(tree, X)
	  
	  val game = Game(Player("X", X, new RandomMoveStrategy()), 
	      Player("O", O, new RandomMoveStrategy()))
	  
	  val status = game.calculateStatus(path.moves.last)

	  assertTrue(status == XWon || status == Tie)
	}

	@Test
	@Ignore
	def testAdviceOWonOrTie() {
	  val tree = buildTree(GameTree.start, 1)
	  val path = generateWinPath(tree, O)
	  
	  assertTrue(path.moves.size > 5)

	  val game = Game(Player("X", X, new RandomMoveStrategy()), 
	      Player("O", O, new RandomMoveStrategy()))
	  
	  val status = game.calculateStatus(path.moves.last)

	  assertTrue(status == OWon || status == Tie)
	}	
	
	@Test
	def winPathsContainTie() {
	  val tree = buildTree(GameTree.start, 1)
	  val paths = GameTree.allAdvices(tree, X)
	  
	  val game = Game(Player("X", X, new RandomMoveStrategy()), 
	      Player("O", O, new RandomMoveStrategy()))	  
	  
	  assertTrue(paths.exists(p => game.calculateStatus(p.moves.last) == Tie))
	}
	
	@Test
	def generateTreeWithConstraint() {
	  val moves =  List((0, 0), (0, 1), (0, 2))
	  val tree = buildTreeWithConstraint(GameTree.start, 1, moves)
	  
	  val paths = GameTree.allAdvicesWithStatus(tree, X, List(NotStarted, Playing, XWon, OWon, Tie))
	  
	  assertEquals(6, paths.length)	  
	}
	
	def generateWinPath(start: GameTree, playerToWin: PlayerType): MovePath = {
	  GameTree.advice(start, playerToWin)
	}
	
	def generateAllMoves2Level(field: Field): List[Field] = {
	  val xmove = field.field(0)
	  
	  val player = Player("Player", O, new RandomMoveStrategy)
	  val moves =  List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))
	  
	  moves.filter(m => m._1 != xmove._2 && m._2 != xmove._3).map(m => field.update(Move(m._1, m._2, player)))	  
	}
	
	def buildTreeWithConstraint(tree: GameTree, level: Int, constraint: List[(Int, Int)]): GameTree = {
	  GameTree.buildWithConstraint(tree, 1, constraint)
	}
	
	def buildTree(tree: GameTree, level: Int): GameTree = {
	    GameTree.build(tree, 1)
	}
}
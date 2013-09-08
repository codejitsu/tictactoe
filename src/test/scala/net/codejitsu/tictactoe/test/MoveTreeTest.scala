package net.codejitsu.tictactoe.test

import org.junit.Assert.assertNotNull
import org.junit.Test
import net.codejitsu.tictactoe.tree.Tree
import net.codejitsu.tictactoe.tree.Step
import net.codejitsu.tictactoe.tree.MoveTree
import net.codejitsu.tictactoe.tree.Step
import net.codejitsu.tictactoe.tree.Fork
import net.codejitsu.tictactoe.Field
import net.codejitsu.tictactoe.PlayerType
import net.codejitsu.tictactoe.tree.Leaf
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.RandomMoveStrategy
import scala.annotation.tailrec
import scala.collection.immutable.HashSet

class MoveTreeTest {
  @Test def testMoveTree() {
    val moves =  List((0, 0), (0, 1), (0, 2))
    
    val p = Player("Player", PlayerType.X, new RandomMoveStrategy())
    
    val firstLevel = MoveTree.collect(Field(), p, Nil, moves)
    
    val allLevels = MoveTree.collectAll(List((Field(), PlayerType.X)), moves, Map.empty)
    val tree: Tree[Step] = MoveTree.make(moves, Fork(Step(Field(), PlayerType.X)), firstLevel, allLevels)
    
    print(tree, 0)
  }
  
  @Test def testFullMoveTree() {
    val moves = List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))
    
    val p = Player("Player", PlayerType.X, new RandomMoveStrategy())
    
    val firstLevel = MoveTree.collect(Field(), p, Nil, moves)
    
    val allLevels = MoveTree.collectAll(List((Field(), PlayerType.X)), moves, Map.empty)
    
    val start = System.currentTimeMillis
    val tree: Tree[Step] = MoveTree.make(moves, Fork(Step(Field(), PlayerType.X)), firstLevel, allLevels)
    
    println("Total time (ms): " + (System.currentTimeMillis - start))
    println()
    
    val leafs = allLeafs(List(tree), 0, HashSet.empty)
    
    println("Total # leafs: " + leafs._1 + " unique: " + leafs._2.size)
  }  
  
  def print(tree: Tree[Step], level: Int): Unit = tree match {
    case n: Fork[Step] => {
      println(s"Level: $level")
      println(n.value.get.field.toString)
      n.children.get.map(ch => print(ch, level + 1))
    }
    case l: Leaf[Step] => {
      println(s"Level: $level")
      println(l.value.get.field.toString)
    }
  }
  
  @tailrec
  private def allLeafs(tree: List[Tree[Step]], acc: Int, all: HashSet[Field]): (Int, HashSet[Field]) = tree match {
    case Nil => (acc, all)
    case x :: xn => x match {
      case n: Fork[Step] => {
        allLeafs(xn ::: n.children.get, acc, all)
      }
      case l: Leaf[Step] => {
        allLeafs(xn, acc + 1, all + l.value.get.field)
      }
    }
  }  
}
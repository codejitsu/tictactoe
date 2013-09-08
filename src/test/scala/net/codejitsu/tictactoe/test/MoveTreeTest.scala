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

class MoveTreeTest {
  @Test def testMoveTree() {
    val moves =  List((0, 0), (0, 1), (0, 2))
    
    val collected = MoveTree.collect(Field(), PlayerType.X, Nil, moves)
    
    val tree: Tree[Step] = MoveTree.make(moves, Fork[Step](Step(Field(), PlayerType.X)), collected)
    
    print(tree, 0)
  }
  
  @Test def testFullMoveTree() {
    val moves = List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))
    
    val collected = MoveTree.collect(Field(), PlayerType.X, Nil, moves)
    
    val tree: Tree[Step] = MoveTree.make(moves, Fork[Step](Step(Field(), PlayerType.X)), collected)
    
    println(tree)
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
}
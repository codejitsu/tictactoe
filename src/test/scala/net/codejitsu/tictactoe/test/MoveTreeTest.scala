package net.codejitsu.tictactoe.test

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import org.junit.Assert.assertEquals
import org.junit.Test
import net.codejitsu.tictactoe.Cell
import net.codejitsu.tictactoe.Field
import net.codejitsu.tictactoe.tree.Fork
import net.codejitsu.tictactoe.tree.Leaf
import net.codejitsu.tictactoe.tree.MoveTree
import net.codejitsu.tictactoe.tree.MoveTree.Step
import net.codejitsu.tictactoe.tree.Tree
import net.codejitsu.tictactoe.PlayerType
import net.codejitsu.tictactoe.tree.MoveTree._

class MoveTreeTest {
  @Test def testMoveTree() {
    val cells =  List(Cell(0, 0), Cell(0, 1), Cell(0, 2))
    
    val tree: Tree[Step] = MoveTree.build(cells = cells)
    
    assertEquals(16, size(List(tree)))
  }
  
  @Test def testCollectPaths() {
    val cells =  List(Cell(0, 0), Cell(0, 1), Cell(0, 2))
    
    val tree: Tree[Step] = MoveTree.build(cells = cells)
    
    val paths = MoveTree.collectPaths(tree, EmptyPath, Nil)

    assertEquals(6, paths.size)
  }
  
  @Test def testFullMoveTree() {
    val start = System.currentTimeMillis
    val tree: Tree[Step] = MoveTree.build()
    
    println("Total time (ms): " + (System.currentTimeMillis - start))
    println()
    
    val leafs = allLeafs(List(tree), 0, HashSet.empty)
    
    println("Total # leafs: " + leafs._1 + " unique: " + leafs._2.size)
  }  
  
  @Test def testCollectPathsFullTree() {
    val tree: Tree[Step] = MoveTree.build()
    
    val paths = MoveTree.collectPaths(tree, EmptyPath, Nil)

    println("Total paths:" + paths.size)
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
  
  @tailrec
  private def size(tree: List[Tree[Step]], acc: Int = 0): Int = tree match {
    case Nil => acc
    case x :: xn => x match {
      case n: Fork[Step] => {
        size(xn ::: n.children.get, acc + 1)
      }
      case l: Leaf[Step] => {
        size(xn, acc + 1)
      }
    }
  }    
}
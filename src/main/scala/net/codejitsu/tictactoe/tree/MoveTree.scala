package net.codejitsu.tictactoe.tree

import net.codejitsu.tictactoe.Field
import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.Move
import net.codejitsu.tictactoe.RandomMoveStrategy
import net.codejitsu.tictactoe.PlayerType
import net.codejitsu.tictactoe.Game
import net.codejitsu.tictactoe.GameStatus._

case class Step(field: Field, player: PlayerType)

object MoveTree {
  import scala.annotation.tailrec
 
  val moves =  List((0, 0), (0, 1), (0, 2))
  
  private val game = Game(Player("X", X, new RandomMoveStrategy),
    Player("O", O, new RandomMoveStrategy))  
  
  @tailrec
  def collect(field: Field, player: PlayerType, 
      acc: List[Field], moves: List[(Int, Int)]): List[Field] = moves match {
    case Nil => acc
    case x :: tail => {
      val (row, col) = x
      val p = Player("Player", player, new RandomMoveStrategy())
      val move = Move(row, col, p)

      if (field.silentVerify(move)) {
        collect(field, player, field.update(move) :: acc, tail)
      } else {
        collect(field, player, acc, tail)
      }
    }
  }  
 
  def nextPlayer(player: PlayerType) = {
    if (player == PlayerType.X) PlayerType.O
    else PlayerType.X
  }  
  
  def isGameOver(field: Field) = {
    val status = game.calculateStatus(field)
    status == OWon || status == XWon || status == Tie  
  }
    
  def build(possibleMoves: List[(Int, Int)], current: Tree[Step]): Tree[Step] = {
    @tailrec
    def buildImpl(possibleMoves: List[(Int, Int)], trees: List[Tree[Step]], 
        product: List[Tree[Step]]): Tree[Step] = trees match {
      case x :: rs => x match {
        case Fork(_) => {
          val n = trees.head
          val next = collect(n.value.get.field, n.value.get.player, Nil, possibleMoves)
          val nextPl = nextPlayer(n.value.get.player)

          val nodes = next.map(node => isGameOver(node) match {
            case false => Fork[Step](Step(node, nextPl))
            case _ => Leaf[Step](Step(node, nextPl))
          })

          val node = Fork[Step](Step(n.value.get.field, n.value.get.player), nodes: _*)
          
          buildImpl(possibleMoves, nodes ::: rs, product :+ node)
        }
        
        case Leaf(_) => buildImpl(possibleMoves, rs, product)
      }
      
      case Nil => product.head
    }
	
	buildImpl(possibleMoves, List[Tree[Step]](current), List[Tree[Step]]())
  }
  
  @tailrec
  def make(possibleMoves: List[(Int, Int)], node: Tree[Step], next: List[Field]): Tree[Step] = next match {
    case Nil => node
    case n :: xn => {
      val nextPl = nextPlayer(node.value.get.player)
      
      val nextLevel = Fork[Step](Step(n, nextPl))
   
      val nextFields = collect(nextLevel.value.get.field, nextLevel.value.get.player, Nil, possibleMoves) 
      
      val nextTree = make2(possibleMoves, nextLevel, nextFields)
      
      val fork = Fork[Step](Step(node.value.get.field, node.value.get.player), (node.children.get :+ nextTree): _*)
      
      make(possibleMoves, fork, xn)
    }
  }
  
  @tailrec
  def make2(possibleMoves: List[(Int, Int)], node: Tree[Step], next: List[Field]): Tree[Step] = next match {
    case Nil => node
    case n :: xn => {
      val nextPl = nextPlayer(node.value.get.player)
      
      val nextLevel = Fork[Step](Step(n, nextPl))
   
      val nextFields = collect(nextLevel.value.get.field, nextLevel.value.get.player, Nil, possibleMoves) 
      
      val nextTree = make(possibleMoves, nextLevel, nextFields)
      
      val fork = Fork[Step](Step(node.value.get.field, node.value.get.player), (node.children.get :+ nextTree): _*)
      
      make2(possibleMoves, fork, xn)
    }
  }  
}
package net.codejitsu.tictactoe.tree

import net.codejitsu.tictactoe.Board
import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.Move
import net.codejitsu.tictactoe.PlayerType
import net.codejitsu.tictactoe.Game
import net.codejitsu.tictactoe.GameStatus
import net.codejitsu.tictactoe.GameStatus._
import net.codejitsu.tictactoe.Cell
import net.codejitsu.tictactoe.PlayStrategy._

object MoveTree {
  import scala.annotation.tailrec
  
  case class Path(steps: List[Step])
  object EmptyPath extends Path(Nil)
  
  case class Step(field: Board, player: PlayerType, status: GameStatus = GameStatus.Playing)
 
  private val cells =  List(
      Cell(0, 0), Cell(0, 1), Cell(0, 2), 
      Cell(1, 0), Cell(1, 1), Cell(1, 2), 
      Cell(2, 0), Cell(2, 1), Cell(2, 2))
  
  private val game = Game(Player("X", X, random),
    Player("O", O, random))  
  
  private lazy val tree = build()  
    
  private lazy val allPaths = collectPaths(tree, EmptyPath, Nil)
  
  @tailrec
  private def collect(board: Board, player: Player, 
      acc: List[Board], moves: List[Cell]): List[Board] = moves match {
    case Nil => acc
    case x :: tail => {
      if (board.verify(Move(x, player.playerType))) {
        collect(board, player, board.update(Move(x, player.playerType)).get :: acc, tail)
      } else {
        collect(board, player, acc, tail)
      }
    }
  }  
 
  private def nextPlayer(player: PlayerType) = {
    if (player == PlayerType.X) PlayerType.O
    else PlayerType.X
  }  
  
  private def isGameOver(field: Board): (GameStatus, Boolean) = {
    val status = game.calculateStatus(field)
    val gameOver = status == OWon || status == XWon || status == Tie
    
    if (gameOver) (status, true)
    else (GameStatus.Playing, false)
  }
  
  @tailrec
  private def make(possibleMoves: List[Cell], 
      node: Tree[Step], next: List[Board], fields: Map[Board, List[Board]]): Tree[Step] = next match {
    case Nil => node
    case n :: xn => {
      lazy val nextPl = nextPlayer(node.value.get.player)
      
      lazy val isOver = isGameOver(n)

      if (!isOver._2) {
        lazy val nextLevel = Fork(Step(n, nextPl))

        lazy val nextFields = fields.get(n).get

        lazy val nextTree = make2(possibleMoves, nextLevel, nextFields, fields)

        lazy val step = node match {
          case f: Fork[Step] => Fork(node.value.get, (node.children.get :+ nextTree): _*)
          case l: Leaf[Step] => Leaf(node.value.get)
        }
        
        make(possibleMoves, step, xn, fields)
      } else {
        lazy val nextLevel = Leaf(Step(n, nextPl, isOver._1))
        
        make(possibleMoves, nextLevel, xn, fields)
      }
    }
  }
  
  @tailrec
  private def make2(possibleMoves: List[Cell], 
      node: Tree[Step], next: List[Board], fields: Map[Board, List[Board]]): Tree[Step] = next match {
    case Nil => node
    case n :: xn => {
     lazy val nextPl = nextPlayer(node.value.get.player)
      
      lazy val isOver = isGameOver(n)

      if (!isOver._2) {
        lazy val nextLevel = Fork(Step(n, nextPl))

        lazy val nextFields = fields.get(n).get

        lazy val nextTree = make(possibleMoves, nextLevel, nextFields, fields)

        lazy val step = node match {
          case f: Fork[Step] => Fork(node.value.get, (node.children.get :+ nextTree): _*)
          case l: Leaf[Step] => Leaf(node.value.get)
        }
        
        make2(possibleMoves, step, xn, fields)
      } else {
        lazy val nextLevel = Leaf(Step(n, nextPl, isOver._1))
        
        make2(possibleMoves, nextLevel, xn, fields)
      }
    }
  }
  
  @tailrec
  private def collectAll(fields: List[(Board, PlayerType)], 
      possibleMoves: List[Cell], all: Map[Board, List[Board]]): Map[Board, List[Board]] = fields match {
    case Nil => all
    case x :: xn => {
      if (!all.contains(x._1)) {
        lazy val p = Player("Player", x._2, random)

        lazy val nextFields = collect(x._1, p, Nil, possibleMoves)

        lazy val nextPl = nextPlayer(x._2)

        lazy val updatedMap = all + (x._1 -> nextFields)

        lazy val withPlayer = nextFields.map(f => (f, nextPl))
        
        collectAll(xn ::: withPlayer, possibleMoves, updatedMap)
      } else {
        collectAll(xn, possibleMoves, all)
      }
    }
  }
  
  def build(player: PlayerType = PlayerType.X, cells: List[Cell] = this.cells) = {
    val p = Player("Player", player, random)
    val firstLevel = MoveTree.collect(Board(), p, Nil, cells)    
	val allLevels = MoveTree.collectAll(List((Board(), player)), cells, Map.empty)
    
	make(cells, Fork(Step(Board(), PlayerType.X)), firstLevel, allLevels)    
  }

  def collectPaths(tree: Tree[Step],
    current: Path, acc: List[Path]): List[Path] = tree match {
    case leaf: Leaf[Step] => {
      current.copy(steps = current.steps :+ leaf.value.get) :: acc
    }
    case fork: Fork[Step] if !fork.children.get.isEmpty => {
      fork.children.get.flatMap(ch => collectPaths(ch, 
          current.copy(steps = current.steps :+ fork.value.get), acc))
    }
    case fork: Fork[Step] if fork.children.get.isEmpty => {
      current.copy(steps = current.steps :+ fork.value.get) :: acc
    }    
  }
  
  def winPaths(player: PlayerType): List[Path] = {
    val status = if (player == X) List(XWon, Tie) else List(OWon, Tie)
    
    allPaths.filter(p => status.contains(p.steps.last.status))
  }
  
  //TODO best path for given tree
  //def advice
}
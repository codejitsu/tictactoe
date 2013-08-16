package net.codejitsu.tictactoe

import net.codejitsu.tictactoe.PlayerType._
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import net.codejitsu.tictactoe.GameStatus._

sealed trait GameTree {
  def nextPlayer: PlayerType
  def toList: List[Field]
  def mkString: String
}

case object Leaf extends GameTree {
  def nextPlayer = null
  def toList = Nil
  def mkString = "-"
}

case class Node(e: Field, children: List[GameTree], nextPlayer: PlayerType) extends GameTree {
  def toList = e :: children.foldLeft(List[Field]())(_ ::: _.toList)

  def mkString = "===" + "\n" + e.toString + "\n" + children.map(_.mkString).foldLeft("")(_ + _)
}

case class MovePath(start: GameTree, moves: List[Field], status: Option[GameStatus])

object GameTree {
  private val game = Game(Player("X", PlayerType.X, new RandomMoveStrategy),
    Player("O", PlayerType.O, new RandomMoveStrategy))
  
  private val all_moves = List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))  
    
  val start = Node(Field(), List[GameTree](), PlayerType.X)

  def findWinOrTiePathsFrom(acc: List[MovePath], current: MovePath, 
      start: GameTree, playerToWin: PlayerType): List[MovePath] = start match {
    case Node(e, ch, p) => {
      ch.flatMap(c => findWinOrTiePathsFrom(acc, current.copy(moves = current.moves :+ e), c, playerToWin))
    }
    
    case Leaf => {
      val lastMove = current.moves.last
      
	  val game = Game(Player("X", PlayerType.X, new RandomMoveStrategy()), 
	      Player("O", PlayerType.O, new RandomMoveStrategy()))
	  
	  val status = game.calculateStatus(lastMove)

	  if(status == GameStatus.Tie) acc :+ current.copy(status = Option(status))
	  else if ((status == GameStatus.XWon && playerToWin == PlayerType.X) || 
	      (status == GameStatus.OWon && playerToWin == PlayerType.O)) {
	    acc :+ current.copy(status = Option(status))
	  } else {
	    acc
	  }
    }
  }
  
  def advice(start: GameTree, playerToWin: PlayerType) : MovePath = {
	val allWinTiePaths = findWinOrTiePathsFrom(List(), MovePath(start, List(), None), start, playerToWin)
    
	val expectedStatus = if (playerToWin == PlayerType.X) GameStatus.XWon else GameStatus.OWon
	
	val allWon = allWinTiePaths.filter(p => p.status.getOrElse(GameStatus.Playing) == expectedStatus)
	
    allWon.head
  }

  private def isGameOver(status: GameStatus) = 
    status == OWon || status == XWon || status == Tie
  
  def build(tree: GameTree, level: Int, upToLevel: Int): GameTree = {
    if (upToLevel == 0) Leaf
    else if (level == upToLevel) tree
    else {
      tree match {
        case Node(e, _, p) => {
          val status = this.game.calculateStatus(e)    
              
          if (this.isGameOver(status)) {
            //TODO save game result in node
            Node(e, List(Leaf), p)
          } else if (upToLevel == FieldSize * FieldSize) tree
          else {
            val player = Player("Player", p, new RandomMoveStrategy())

            val fields = collectFields(e, player, List.empty[Field], this.all_moves)

            val children = fields.map(Node(_, List[GameTree](), nextPlayer(p)))

            Node(e, children.map(build(_, level + 1, upToLevel)), p)
          }
        }

        case _ => Leaf
      }
    }
  }

  private def collectFields(parentField: Field, player: Player, 
      acc: List[Field], moves: List[(Int, Int)]): List[Field] = moves match {
    case Nil => acc
    case x :: tail => {
      val (row, col) = x
      val move = Move(row, col, player)

      if (parentField.silentVerify(move)) {
        collectFields(parentField, player, parentField.update(move) :: acc, tail)
      } else {
        collectFields(parentField, player, acc, tail)
      }
    }
  }

  def nextPlayer(player: PlayerType) = {
    if (player == PlayerType.X) PlayerType.O
    else PlayerType.X
  }

  def print(tree: GameTree, startLevel: Int): Unit = tree match {
    case Leaf => println("[]")
    case Node(e, ch, p) => {
      println("Level: " + startLevel)
      val field = e
      println(field.toString)

      ch.map(c => print(c, startLevel + 1))
    }
  }

  def printLevel(tree: GameTree, level: Int, targetLevel: Int): Unit = tree match {
    case Leaf => println("[]")
    case Node(e, ch, p) => {
      if (level == targetLevel - 1) {
        println("Level: " + targetLevel)
        ch.map(println(_))
      } else {
        ch.map(c => printLevel(c, level + 1, targetLevel))
      }
    }
  }
}
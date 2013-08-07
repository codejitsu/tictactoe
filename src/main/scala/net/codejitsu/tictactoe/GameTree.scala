package net.codejitsu.tictactoe

import net.codejitsu.tictactoe.PlayerType._
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import net.codejitsu.tictactoe.GameStatus._

sealed trait GameTree[+T] {
  def nextPlayer: PlayerType
  def toList: List[T]
  def mkString: String
}

case object Leaf extends GameTree[Nothing] {
  def nextPlayer = null
  def toList = Nil
  def mkString = "-"
}

case class Node[T](e: T, children: List[GameTree[T]], nextPlayer: PlayerType) extends GameTree[T] {
  def toList = e :: children.foldLeft(List[T]())(_ ::: _.toList)

  def mkString = e match {
    case Some(s) => "===" + "\n" + s.toString + "\n" + children.map(_.mkString).foldLeft("")(_ + _)
    case _ => "===" + "\n" + "[]" + "\n" + children.map(_.mkString).foldLeft("")(_ + _)
  }
}

case class MovePath(start: GameTree[Field], moves: List[Field], status: Option[GameStatus])

object GameTree {
  def start: GameTree[Field] = {
    Node[Field](Field(), List[GameTree[Field]](), PlayerType.O)
  }

  def findWinOrTiePathsFrom(acc: List[MovePath], current: MovePath, 
      start: GameTree[Field], playerToWin: PlayerType): List[MovePath] = start match {
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
  
  def advice(start: GameTree[Field], playerToWin: PlayerType) : MovePath = {
	val allWinTiePaths = findWinOrTiePathsFrom(List(), MovePath(start, List(), None), start, playerToWin)
    
	val expectedStatus = if (playerToWin == PlayerType.X) GameStatus.XWon else GameStatus.OWon
	
	val allWon = allWinTiePaths.filter(p => p.status.getOrElse(GameStatus.Playing) == expectedStatus)
	
    allWon.head
  }
  
  def build(tree: GameTree[Field], level: Int, upToLevel: Int): GameTree[Field] = {
    if (upToLevel == 0) Leaf
    else if (level == upToLevel) tree
    else {
      tree match {
        case Node(e, ch, p) => {
          val parent = e

          val game = Game(Player("X", PlayerType.X, new RandomMoveStrategy), 
              Player("O", PlayerType.O, new RandomMoveStrategy))
          
          val status = game.calculateStatus(parent)    
              
          if (status == GameStatus.OWon || status == GameStatus.XWon || status == GameStatus.Tie) {
            Node[Field](e, List(Leaf), p)
          } else if (upToLevel == FieldSize * FieldSize) tree
          else {
            val totalMoves = FieldSize * FieldSize - upToLevel

            val parentField = e

            val player = Player("Player", p, new RandomMoveStrategy())

            val fields = collectFields(parentField, player, HashSet.empty[String], List.empty[Field], totalMoves,
              moves)

            val children = fields.map(f => Node[Field](f, List[GameTree[Field]](), nextPlayer(p)))

            Node[Field](e, children.map(build(_, level + 1, upToLevel)), nextPlayer(p))
          }
        }

        case _ => Leaf
      }
    }
  }

  def moves = List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))

  def collectFields(parentField: Field, player: Player,
    hash: HashSet[String], acc: List[Field], total: Int, moves: List[(Int, Int)]): List[Field] = moves match {
    case Nil => acc
    case x :: tail => {
      val move = Move(x._1, x._2, player)

      if (parentField.silentVerify(move)) {
        val nextField = parentField.update(move)

        if (!hash.contains(nextField.toString)) {
          collectFields(parentField, player, hash + nextField.toString, nextField :: acc, total - 1, tail)
        } else {
          collectFields(parentField, player, hash, acc, total - 1, tail)
        }
      } else collectFields(parentField, player, hash, acc, total - 1, tail)
    }
  }

  def nextPlayer(player: PlayerType) = {
    if (player == PlayerType.X) PlayerType.O
    else PlayerType.X
  }

  def print(tree: GameTree[Field], startLevel: Int): Unit = tree match {
    case Leaf => println("[]")
    case Node(e, ch, p) => {
      println("Level: " + startLevel)
      val field = e
      println(field.toString)

      ch.map(c => print(c, startLevel + 1))
    }
  }

  def printLevel(tree: GameTree[Field], level: Int, targetLevel: Int): Unit = tree match {
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
package net.codejitsu.tictactoe

import scala.collection.immutable.Stream.consWrapper

import net.codejitsu.tictactoe.GameStatus.GameStatus
import net.codejitsu.tictactoe.GameStatus.OWon
import net.codejitsu.tictactoe.GameStatus.Playing
import net.codejitsu.tictactoe.GameStatus.Tie
import net.codejitsu.tictactoe.GameStatus.XWon
import net.codejitsu.tictactoe.PlayerType.O
import net.codejitsu.tictactoe.PlayerType.PlayerType
import net.codejitsu.tictactoe.PlayerType.X

sealed trait GameTree {
  def nextPlayer: PlayerType
  def toList: List[Field]
  def mkString: String
}

case class Leaf(gameStatus: GameStatus) extends GameTree {
  def nextPlayer = null
  def toList = Nil
  def mkString = "-"
}

case class Node(e: Field, children: List[GameTree], nextPlayer: PlayerType, gameStatus: GameStatus) extends GameTree {
  def toList = e :: children.foldLeft(List[Field]())(_ ::: _.toList)

  def mkString = "===" + "\n" + e.toString + "\n" + children.map(_.mkString).foldLeft("")(_ + _)
}

case class MovePath(start: GameTree, moves: Stream[Field], status: Option[GameStatus])
object EmptyPath extends MovePath(Leaf(Playing), Stream.empty, Option(Playing))

object GameTree {
  val start = Node(Field(), List[GameTree](), X, Playing)
  
  private val game = Game(Player("X", X, new RandomMoveStrategy),
    Player("O", O, new RandomMoveStrategy))
  
  private val all_moves = List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))  

  private def findWinOrTiePathsFrom(current: MovePath, 
      startNode: GameTree, playerToWin: PlayerType): Stream[MovePath] = startNode match {
    case Node(e, ch, p, _) => {
      Stream.range(0, ch.size) flatMap (i => 
        findWinOrTiePathsFrom(current.copy(moves = current.moves :+ e), ch(i), playerToWin))
    }
    
    case Leaf(status) => {
	  if(status == Tie) current.copy(status = Option(status)) #:: Stream.empty
	  else 
	  if ((status == XWon && playerToWin == X) || 
	      (status == OWon && playerToWin == O)) {
	    current.copy(status = Option(status)) #:: Stream.empty
	  } else {
	    Stream.empty
	  }
    }
  }
  
  def allAdvices(startNode: GameTree, playerToWin: PlayerType) : Stream[MovePath] = {
	val allWinTiePaths = findWinOrTiePathsFrom(MovePath(startNode, Stream.empty, None), startNode, playerToWin)
    
	val expectedStatuses = if (playerToWin == X) List(XWon, Tie) else List(OWon, Tie)
	
	allWinTiePaths.filter(p => expectedStatuses.contains(p.status.getOrElse(Playing)))   
  }
  
  def advice(startNode: GameTree, playerToWin: PlayerType) : MovePath = {
	val allWinTiePaths = allAdvices(startNode, playerToWin)
	
	def findShortestWinPath(paths: Iterator[MovePath]): MovePath = {
	  var bestMatch: MovePath = EmptyPath
	  
	  for(x <- paths) {
        if (x.status == XWon && playerToWin == X || x.status == OWon && playerToWin == O) { 
		  if (x.moves.size <= bestMatch.moves.size) {
		    bestMatch = x
		  } 
	    }
	  }
	  
	  bestMatch
	}
	
    findShortestWinPath(allWinTiePaths.toIterator)
  }

  private def isGameOver(status: GameStatus) = 
    status == OWon || status == XWon || status == Tie
  
  private def isTreeCompleted(tree: GameTree): Boolean = tree match {
    case Leaf(_) => true
    case Node(_, ch, _, _) => {
      if (ch.isEmpty) false
      else ch.forall(isTreeCompleted(_))
    }
  }  
    
  def build(tree: GameTree, level: Int, upToLevel: Int): GameTree = {
    if (upToLevel == 0) throw new IllegalArgumentException("0")
    //else if (level == upToLevel) tree
    else if (isTreeCompleted(tree)) {
      tree
    } else {
      tree match {
        case Node(e, ch, p, s) => {
         // if (e.field.size > 8) println("wow!")
          
          if (this.isGameOver(s)) {
            build(Node(e, List(Leaf(s)), p, Playing), level + 1, upToLevel)
          } else {
            val player = Player("Player", p, new RandomMoveStrategy())

            val fields = collectFields(e, player, List.empty[Field], this.all_moves)

            val nextPl = nextPlayer(p)
            
            val children = fields.map(f => build(Node(f, List[GameTree](), 
                nextPl, this.game.calculateStatus(f)), level + 1, upToLevel))

            Node(e, children, p, s)
          }
        }

        case Leaf(s) => Leaf(s)
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
    case Leaf(s) => println("[]: " + s)
    case Node(e, ch, p, _) => {
      println("Level: " + startLevel)
      val field = e
      println(field.toString)

      ch.map(c => print(c, startLevel + 1))
    }
  }

  def printLevel(tree: GameTree, level: Int, targetLevel: Int): Unit = tree match {
    case Leaf(s) => println("[]: " + s)
    case Node(e, ch, p, _) => {
      if (level == targetLevel - 1) {
        println("Level: " + targetLevel)
        ch.map(println(_))
      } else {
        ch.map(c => printLevel(c, level + 1, targetLevel))
      }
    }
  }
}
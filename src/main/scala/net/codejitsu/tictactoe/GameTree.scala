package net.codejitsu.tictactoe

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
  def nodes: List[GameTree]
}

case class Leaf(gameStatus: GameStatus) extends GameTree {
  def nextPlayer = null
  def toList = Nil
  def mkString = "-"
  def nodes = Nil
}

case class Node(e: Field, children: List[GameTree], nextPlayer: PlayerType, gameStatus: GameStatus) extends GameTree {
  def toList = e :: children.foldLeft(List[Field]())(_ ::: _.toList)
  def mkString = "===" + "\n" + e.toString + "\n" + children.map(_.mkString).foldLeft("")(_ + _)
  def nodes = children
}

case class MovePath(start: GameTree, moves: List[Field], status: Option[GameStatus])
object EmptyPath extends MovePath(Leaf(Playing), Nil, Option(Playing))

object GameTree {
  val start = Node(Field(), List[GameTree](), X, Playing)
  
  private val game = Game(Player("X", X, new RandomMoveStrategy),
    Player("O", O, new RandomMoveStrategy))
  
  private val all_moves = List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))  

  private def findPathsFrom(current: MovePath, 
      startNode: GameTree, playerToWin: PlayerType, acc: List[MovePath]): List[MovePath] = startNode match {
    case Node(e, children, p, s) => {
      if (children.isEmpty) {
        acc :+ current.copy(moves = current.moves :+ e)
      } else {
        children.flatMap(ch => findPathsFrom(current.copy(moves = current.moves :+ e), 
            ch, playerToWin, acc))
      }
    }
    
    case Leaf(s) => acc :+ current
  }
  
  def allAdvices(startNode: GameTree, playerToWin: PlayerType) : List[MovePath] = {
    val expectedStatuses = if (playerToWin == X) List(XWon, Tie) else List(OWon, Tie)
    allAdvicesWithStatus(startNode, playerToWin, expectedStatuses)
  }
  
  def allAdvicesWithStatus(startNode: GameTree, playerToWin: PlayerType, 
      expectedStatuses: List[GameStatus]) : List[MovePath] = {
	val allWinTiePaths = findPathsFrom(EmptyPath, startNode, playerToWin, Nil)
    
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
    
  def build(tree: GameTree, level: Int) = {
    buildWithConstraint(tree, level, this.all_moves)
  }
  
  def buildWithConstraint(tree: GameTree, level: Int,
      constraint: List[(Int, Int)]): GameTree = {
    if (isTreeCompleted(tree)) {
      tree
    } else {
      tree match {
        case Node(e, ch, p, s) => {
          if (this.isGameOver(s)) {
            build(Node(e, List(Leaf(s)), p, Playing), level + 1)
          } else {
            val player = Player("Player", p, new RandomMoveStrategy())

            val fields = collectFields(e, player, List.empty[Field], constraint)

            val nextPl = nextPlayer(p)
            
            val children = fields.map(f => buildWithConstraint(Node(f, List[GameTree](), 
                nextPl, this.game.calculateStatus(f)), level + 1, constraint))

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
    case Leaf(s) => println("Leaf => " + s)
    case Node(e, ch, p, _) => {
      println("Level: " + startLevel)
      val field = e
      println(field.toString)

      ch.map(c => print(c, startLevel + 1))
    }
  }

  def printLevel(tree: GameTree, level: Int, targetLevel: Int): Unit = tree match {
    case Leaf(s) => println("Leaf => " + s)
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
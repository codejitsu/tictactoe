package net.codejitsu.tictactoe

import scala.annotation.tailrec
import scala.collection.immutable.Stream.consWrapper
import scala.collection.mutable.ListBuffer

import net.codejitsu.tictactoe.GameStatus.GameStatus
import net.codejitsu.tictactoe.GameStatus.NotStarted
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
  val nodes: Stream[GameTree]
  def status: GameStatus
  def parent: GameTree
  def field: Field
  def id: Int
}

case class Leaf(e: Field, gameStatus: GameStatus, par: GameTree, id: Int) extends GameTree {
  def nextPlayer = null
  def toList = Nil
  def mkString = "-"
  val nodes = Stream.Empty
  def status = gameStatus
  def parent = par
  def field = e
}

case class Node(e: Field, par: GameTree, children: Stream[GameTree], nextPlayer: PlayerType, gameStatus: GameStatus, id: Int) extends GameTree {
  def toList = e :: children.foldLeft(List[Field]())(_ ::: _.toList)
  def mkString = "===" + "\n" + e.toString + "\n" + children.map(_.mkString).foldLeft("")(_ + _)
  lazy val nodes = children
  lazy val status = gameStatus
  def parent = par
  def field = e
}

object Root extends GameTree {
  def nextPlayer = null
  def toList = Nil
  def mkString = "-"
  val nodes = Stream.Empty
  def status = NotStarted
  def parent = Root 
  def field = Field()
  val id = 0
}

object Sink extends GameTree {
  def nextPlayer = null
  def toList = Nil
  def mkString = "-"
  val nodes = Stream.Empty
  def status = NotStarted
  def parent = Sink  
  def field = Field()
  val id = -1
}

case class MovePath(start: GameTree, moves: Stream[Field], status: Option[GameStatus])
object EmptyPath extends MovePath(Root, Stream.empty[Field], Option(Playing))

object GameTree {
  val start = Node(Field(), Root, Stream.empty[GameTree], X, Playing, 0)
  
  val leafsXWon = ListBuffer.empty[GameTree]
  val leafsOWon = ListBuffer.empty[GameTree]
  val leafsTie = ListBuffer.empty[GameTree]
  
  var startId = 0
  
  private val game = Game(Player("X", X, new RandomMoveStrategy),
    Player("O", O, new RandomMoveStrategy))
  
  private val all_moves = List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))

  def getAllLeafPaths(node: GameTree): List[List[GameTree]] = 
    getAllLeafPathsFromList(List.empty[List[GameTree]], List(node), node.nodes)

  @tailrec
  def getAllLeafPathsFromList(accPaths: List[List[GameTree]], current: List[GameTree],
    children: Stream[GameTree]): List[List[GameTree]] = children match {
    case head #:: tail => head match {
      case node @ Node(f, p, ch, _, _, id) => {
        if (ch.isEmpty) {
          val path = node :: current

          val moreThanTwo = tail match {
            case x #:: xxx => true
            case _ => false
          }

          if (moreThanTwo && !current.isEmpty && tail.head.parent.id != current.head.id) {
            val dropped_current = current.dropWhile(p => p.id != tail.head.parent.id)
            getAllLeafPathsFromList(path :: accPaths, dropped_current, tail)
          } else {
            getAllLeafPathsFromList(path :: accPaths, current, tail)
          }
        } else {
          getAllLeafPathsFromList(accPaths, node :: current, ch #::: tail)
        }
      }
      case leaf @ Leaf(f, g, par, _) => {
        val path = leaf :: current

        val moreThanTwo = tail match {
          case x #:: xxx => true
          case _ => false
        }
        
        if (moreThanTwo && !current.isEmpty && tail.head.parent.id != current.head.id) {
          val dropped_current = current.dropWhile(p => p.id != tail.head.parent.id)
          getAllLeafPathsFromList(path :: accPaths, dropped_current, tail)
        } else {
          getAllLeafPathsFromList(path :: accPaths, current, tail)
        }
      }
    }

    case Stream.Empty => accPaths
  }
  
  def allAdvices(startNode: GameTree, playerToWin: PlayerType) : Stream[MovePath] = {
    val expectedStatuses = if (playerToWin == X) List(XWon, Tie) else List(OWon, Tie)
    allAdvicesWithStatus(startNode, playerToWin, expectedStatuses)
  }
  
  def allAdvicesWithStatus(startNode: GameTree, playerToWin: PlayerType, 
      expectedStatuses: List[GameStatus]) : Stream[MovePath] = {
   // val allPaths = findPaths(startNode, startNode.nodes, List(), List()).map(t => MovePath(Root, t.toStream, Option(Playing))).toStream
    val allTrees = getAllLeafPaths(startNode)
    val allPaths = allTrees.map(t => t.map(_.field)).map(t => MovePath(Root, t.toStream, Option(Playing))).toStream
    
	allPaths.filter(p => expectedStatuses.contains(game.calculateStatus(p.moves.head)))   
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
    case Leaf(_, _, _, _) => true
    case Node(_, _, ch, _, _, _) => {
      if (ch.isEmpty) false
      else ch.forall(isTreeCompleted(_))
    }
  }  
    
  def build(tree: GameTree) = {
    this.leafsOWon.clear()
    this.leafsXWon.clear()
    this.leafsTie.clear()
    
    this.startId = 0

    buildWithConstraint(tree, this.all_moves)
//    val player = Player("Player", tree.nextPlayer, new RandomMoveStrategy())
//    val fields = collectFields(tree.field, player, List.empty[Field], this.all_moves)    
//    
//    buildWithConstraintRec(this.all_moves, fields, tree)
  }
  
  def buildWithConstraint(tree: GameTree, constraint: List[(Int, Int)]): GameTree = {
    if (isTreeCompleted(tree)) {
      tree
    } else {
      tree match {
        case node@Node(e, par, ch, p, s, i) => {
          if (this.isGameOver(s)) {
            startId = startId + 1
            buildWithConstraint(Node(e, par, Stream.empty[GameTree] :+ Leaf(e, s, par, startId), p, Playing, i), constraint)
          } else {
            val player = Player("Player", p, new RandomMoveStrategy())

            val fields = collectFields(e, player, List.empty[Field], constraint).toStream

            val nextPl = nextPlayer(p)

            val idrange = startId + 1 to startId + fields.size
            
            startId = startId + fields.size + 1
            
            val withid = fields.zip(idrange)
            
            val children = withid.map(w => buildWithConstraint(Node(w._1, node, Stream.empty[GameTree], 
                nextPl, this.game.calculateStatus(w._1), w._2), constraint))

            Node(e, par, children, p, s, i)
          }
        }

        case Leaf(e, s, par, i) => Leaf(e, s, par, i)
      }
    }
  }

  @tailrec
  def buildWithConstraintRec(constraint: List[(Int, Int)], nodes: List[Field], currentTree: GameTree): GameTree = nodes match {
    case head :: tail => currentTree match {
      case node@Node(f, par, children, next, gameStatus, id) => {
       if (this.isGameOver(gameStatus)) {
         startId = startId + 1
         node.copy(children = Leaf(head, gameStatus, node, startId) #:: children)
       } else {
         val player = Player("Player", next, new RandomMoveStrategy())
         val fields = collectFields(f, player, List.empty[Field], constraint)
         val nextPl = nextPlayer(next)         
         
         startId = startId + 1
         val nextNode = Node(head, node, Stream.Empty, nextPl, gameStatus, startId)
         
         val updated = node.copy(children = nextNode #:: children)
         
         buildWithConstraintRec(constraint, fields, updated)
       }
      }
      case Leaf(f, gameStatus, par, id) => currentTree
    }
    
    case _ => currentTree
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
    case Leaf(_, s, _, _) => println("Leaf => " + s)
    case Node(e, _, ch, p, _, _) => {
      println("Level: " + startLevel)
      val field = e
      println(field.toString)

      ch.map(c => print(c, startLevel + 1))
    }
  }

  def printLevel(tree: GameTree, level: Int, targetLevel: Int): Unit = tree match {
    case Leaf(_, s, _, _) => println("Leaf => " + s)
    case Node(e, _, ch, p, _, _) => {
      if (level == targetLevel - 1) {
        println("Level: " + targetLevel)
        ch.map(println(_))
      } else {
        ch.map(c => printLevel(c, level + 1, targetLevel))
      }
    }
  }
}
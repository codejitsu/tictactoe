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
}

case class Leaf(e: Field, gameStatus: GameStatus, par: GameTree) extends GameTree {
  def nextPlayer = null
  def toList = Nil
  def mkString = "-"
  val nodes = Stream.Empty
  def status = gameStatus
  def parent = par
  def field = e
}

case class Node(e: Field, par: GameTree, children: Stream[GameTree], nextPlayer: PlayerType, gameStatus: GameStatus) extends GameTree {
  def toList = e :: children.foldLeft(List[Field]())(_ ::: _.toList)
  def mkString = "===" + "\n" + e.toString + "\n" + children.map(_.mkString).foldLeft("")(_ + _)
  lazy val nodes = children
  def status = gameStatus
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
}

object Sink extends GameTree {
  def nextPlayer = null
  def toList = Nil
  def mkString = "-"
  val nodes = Stream.Empty
  def status = NotStarted
  def parent = Sink  
  def field = Field()
}

case class MovePath(start: GameTree, moves: Stream[Field], status: Option[GameStatus])
object EmptyPath extends MovePath(Root, Stream.empty[Field], Option(Playing))

object GameTree {
  val start = Node(Field(), Root, Stream.empty[GameTree], X, Playing)
  
  val leafsXWon = ListBuffer.empty[GameTree]
  val leafsOWon = ListBuffer.empty[GameTree]
  val leafsTie = ListBuffer.empty[GameTree]
  
  private val game = Game(Player("X", X, new RandomMoveStrategy),
    Player("O", O, new RandomMoveStrategy))
  
  private val all_moves = List((0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2))  

  private def findPathsFrom(startNode: GameTree, children: Stream[GameTree], current: Stream[Field],
      playerToWin: PlayerType, acc: Stream[MovePath]): Stream[MovePath] = startNode match {
    case Leaf(_, _, _) => {
      MovePath(Root, current, Option(Playing)) #:: acc
    }

    case Node(f, _, _, _, _) => {
      if (children.isEmpty) {
        MovePath(Root, current, Option(Playing)) #:: acc
      } else {
        val appended = f #:: current
        children.flatMap(c => findPathsFrom(c, c.nodes, appended, playerToWin, acc)) 
      }
    }
  }
 
  @tailrec
  private def findPathsMap(startNode: GameTree, children: Stream[GameTree], 
      acc: List[List[Field]]): List[List[Field]] = children match {
    case Stream.Empty => acc
    case all@(x #:: tail) => {
      val last = acc.find(p => p.last.field == startNode.field.field).getOrElse(List())
      
      if (last.isEmpty) {
        val allFields = all.map(_.field).toList.map(last :+ _)
        findPathsMap(x, x.nodes, acc ::: allFields)
      } else {
    	val index = acc.indexOf(last)
        val removed = acc.drop(index)
        val allFields = all.map(_.field).toList.map(last :+ _)
        
        findPathsMap(startNode, tail, removed ::: allFields)
      }
    }
  }

  def findPaths(currentNode: GameTree, children: Stream[GameTree],
    currentPath: List[Field], paths: List[List[Field]]): List[List[Field]] = {
    if (currentNode.nodes.isEmpty) {
      currentPath :: paths
    } else {
      children match {
        case Stream.Empty => {
          paths
        }
        case x #:: tail => {
          findPaths(x, x.nodes, x.field :: currentPath, paths) ::: findPaths(currentNode, tail, currentPath, paths)
        }
      }
    }
  }
  
  def getAllLeafPaths(node: GameTree): List[List[Field]] = getAllLeafPathsFromList(List.empty[List[Field]], List(node.field), node.nodes)
  
  @tailrec
  def getAllLeafPathsFromList(accPaths: List[List[Field]], current: List[Field], children: Stream[GameTree]): List[List[Field]] = children match {
   case head #:: tail => head match {
      case Node(f, p, ch, _, _) => {
        println("Element:");
        println(f.toString);
        println("Element->Parent:");
        println(p.field.toString);

        println("Current.Head:");
        println(current.head.toString);
        
    	if (ch.isEmpty) {
          //LEAF
    	  
    	  println("> Leaf");
    	  
    	  println("Parent -> children size: " + p.nodes.length);
    	  
    	  if (head.parent.field != current.head) {
    	    println("Current tree completed! Go back.");
    	    getAllLeafPathsFromList((head.field :: current) :: accPaths, current.tail, tail)
    	  } else {
    	    println("Current tree not completed yet.");
    	    getAllLeafPathsFromList((head.field :: current) :: accPaths, current, tail)
    	  }
    	} else {
    	  //NODE
    	  
    	  println("> Node");
    	  if (head.parent.field != current.head || p.nodes.size == 1) {
            println("Current tree completed! Go back.");
    	    getAllLeafPathsFromList(accPaths, head.field :: current.tail, ch #::: tail)
    	  } else {
    	    println("Current tree not completed yet.");
    	    getAllLeafPathsFromList(accPaths, head.field :: current, ch #::: tail)
    	  }
    	}
      }
      case Leaf(f, g, par) => {
        getAllLeafPathsFromList((head.field :: current) :: accPaths, current.tail, tail)
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
    val allPaths = getAllLeafPaths(startNode).map(t => MovePath(Root, t.toStream, Option(Playing))).toStream
    
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
    case Leaf(_, _, _) => true
    case Node(_, _, ch, _, _) => {
      if (ch.isEmpty) false
      else ch.forall(isTreeCompleted(_))
    }
  }  
    
  def build(tree: GameTree, level: Int) = {
    this.leafsOWon.clear()
    this.leafsXWon.clear()
    this.leafsTie.clear();
    
    buildWithConstraint(tree, level, this.all_moves)
  }
  
  def buildWithConstraint(tree: GameTree, level: Int,
      constraint: List[(Int, Int)]): GameTree = {
    if (isTreeCompleted(tree)) {
      if (tree.status == XWon) this.leafsXWon += tree
      if (tree.status == OWon) this.leafsOWon += tree
      if (tree.status == Tie) this.leafsTie += tree
      
      tree
    } else {
      tree match {
        case node@Node(e, par, ch, p, s) => {
          if (this.isGameOver(s)) {
            buildWithConstraint(Node(e, par, Stream.empty[GameTree] :+ Leaf(e, s, par), p, Playing), level + 1, constraint)
          } else {
            val player = Player("Player", p, new RandomMoveStrategy())

            val fields = collectFields(e, player, List.empty[Field], constraint).toStream

            val nextPl = nextPlayer(p)
            
            val children = fields.map(f => buildWithConstraint(Node(f, node, Stream.empty[GameTree], 
                nextPl, this.game.calculateStatus(f)), level + 1, constraint))

            Node(e, par, children, p, s)
          }
        }

        case Leaf(e, s, par) => Leaf(e, s, par)
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
    case Leaf(_, s, _) => println("Leaf => " + s)
    case Node(e, _, ch, p, _) => {
      println("Level: " + startLevel)
      val field = e
      println(field.toString)

      ch.map(c => print(c, startLevel + 1))
    }
  }

  def printLevel(tree: GameTree, level: Int, targetLevel: Int): Unit = tree match {
    case Leaf(_, s, _) => println("Leaf => " + s)
    case Node(e, _, ch, p, _) => {
      if (level == targetLevel - 1) {
        println("Level: " + targetLevel)
        ch.map(println(_))
      } else {
        ch.map(c => printLevel(c, level + 1, targetLevel))
      }
    }
  }
}
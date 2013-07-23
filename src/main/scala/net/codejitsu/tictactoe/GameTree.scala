package net.codejitsu.tictactoe

import net.codejitsu.tictactoe.PlayerType._
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet

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

case class Node[T](e: Option[T], children: List[GameTree[T]], nextPlayer: PlayerType) extends GameTree[T] {
  def toList = e match {
    case Some(s) => s :: children.foldLeft(List[T]())(_ ::: _.toList)
    case _ => children.foldLeft(List[T]())(_ ::: _.toList)
  }

  def mkString = e match {
    case Some(s) => "===" + "\n" + s.toString + "\n" + children.map(_.mkString).foldLeft("")(_ + _)
    case _ => "===" + "\n" + "[]" + "\n" + children.map(_.mkString).foldLeft("")(_ + _)
  }
}

object GameTree {
  def start: GameTree[Field] = {
    Node[Field](Option(Field()), List[GameTree[Field]](), PlayerType.X)
  }

  def build(tree: GameTree[Field], level: Int, upToLevel: Int): GameTree[Field] = {
    if (upToLevel == 0) Leaf
    else if (level == upToLevel) tree
    else {
      tree match {
        case Node(e, ch, p) => {
          if (upToLevel == FieldSize * FieldSize) tree
          else {
            val totalMoves = FieldSize * FieldSize - upToLevel

            val parentField = e.getOrElse(null)

            val player = Player("Player", p, new RandomMoveStrategy())

            val fields = collectFields(parentField, player, HashSet.empty[String], List.empty[Field], totalMoves,
              moves)

            val children = fields.map(f => Node[Field](Option(f), List[GameTree[Field]](), nextPlayer(p)))

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
    case x :: tail =>
      try {
        val nextField = parentField.update(Move(x._1, x._2, player))

        if (!hash.contains(nextField.toString)) {
          collectFields(parentField, player, hash + nextField.toString, nextField :: acc, total - 1, tail)
        } else {
          collectFields(parentField, player, hash, acc, total - 1, tail)
        }
      } catch {
        case ise: IllegalStateException => collectFields(parentField, player, hash, acc, total - 1, tail)
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
      val field = e.getOrElse(Field())
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
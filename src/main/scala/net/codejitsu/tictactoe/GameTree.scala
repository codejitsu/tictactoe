package net.codejitsu.tictactoe

import net.codejitsu.tictactoe.PlayerType._

sealed trait GameTree[+T] {
  def nextPlayer: PlayerType
  def toList: List[T]
}

case object Leaf extends GameTree[Nothing] {
  def nextPlayer = null
  def toList = Nil
}

case class Node[T](e: Option[T], children: List[GameTree[T]], nextPlayer: PlayerType) extends GameTree[T] {
  def toList = e match {
    case Some(s) => s :: children.foldLeft(List[T]())(_ ::: _.toList)
    case _ => children.foldLeft(List[T]())(_ ::: _.toList)
  }
}

object GameTree {
  def start: GameTree[Field] = {
    Node[Field](Option(Field()), List[GameTree[Field]](), PlayerType.X)
  }

  def build(tree: GameTree[Field], level: Int): GameTree[Field] = {
    if (level == 0) Leaf
    else {
      tree match {
        case Node(e, ch, p) => {
          if (level == FieldSize * FieldSize) tree
          else {
            val totalMoves = FieldSize * FieldSize - level

            val parentField = e.getOrElse(null)

            val player = Player("Player", p, new RandomMoveStrategy())

            val map = scala.collection.mutable.HashMap.empty[String, Boolean]
            val buf = scala.collection.mutable.ListBuffer.empty[Field]
            
            for (step <- 1 to totalMoves) {
              for (i <- 0 until FieldSize) {
                for (j <- 0 until FieldSize) {
                  try {
                    val nextField = parentField.update(Move(i, j, player))
                    
                    if (!map.contains(nextField.toString)) {
                      map += (nextField.toString -> true)
                      buf += nextField
                    }
                  } catch {
                    case ise: IllegalStateException => ()
                  }
                }
              }
            }

            val children = buf.toList.map(f => Node[Field](Option(f), List[GameTree[Field]](), nextPlayer(p)))
            
            Node[Field](e, children.map(build(_, level - 1)), nextPlayer(p))
          }
        }

        case _ => Leaf
      }
    }
  }

  def nextPlayer(player: PlayerType) = {
    if (player == PlayerType.X) PlayerType.O
    else PlayerType.X
  }
}
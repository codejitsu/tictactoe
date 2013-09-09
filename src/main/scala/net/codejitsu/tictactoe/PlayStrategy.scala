package net.codejitsu.tictactoe

import scala.util.Random
import scala.annotation.tailrec
import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.GameStatus._

trait PlayStrategy {
  def makeMove(field: Field, player: Player): Move
}

class RandomMoveStrategy extends PlayStrategy {
  private val rand = new Random()
  def makeMove(field: Field, player: Player) = 
    Move(Cell(this.rand.nextInt(FieldSize), this.rand.nextInt(FieldSize)), player)
}

class ReadConsoleStrategy extends PlayStrategy {
  @tailrec
  private def doMove(field: Field, player: Player): Move = {
    print(player.playerType + ": ")

    val input = readLine()

    println()

    if (!verifyInput(input)) {
      println("Invalid input.")
      doMove(field, player)
    } else {
      val coordinates = parseCoordinates(input)
      Move(Cell(coordinates._1, coordinates._2), player)
    }
  }

  def makeMove(field: Field, player: Player): Move = {
    doMove(field, player)
  }

  def verifyInput(input: String): Boolean = {
    if (input == null) false
    else if (!input.contains(",")) false
    else if (!input.forall(c => c.isDigit || c == ',')) false
    else if (input.count(c => c == ',') > 1) false
    else if (!input.split(",").map(_.toInt).forall(c => c >= 0 && c < FieldSize)) false
    else true
  }

  def parseCoordinates(input: String): (Int, Int) = {
    val coords = input.split(",").map(_.toInt)
    (coords(0), coords(1))
  }
}

class GodStrategy extends PlayStrategy {
  private lazy val gameTree: GameTree = buildGameTree(Node(Field(), Root, Stream.Empty, X, Playing, 0), 0, X)

  private def buildGameTree(tree: GameTree, 
      level: Int, currentPlayer: PlayerType): GameTree = {
    if (level < FieldSize * FieldSize) {
      val nextPlayer = if (currentPlayer == X) O else X
      buildGameTree(buildGameTreeLevel(tree, level, currentPlayer), level + 1, nextPlayer)
    } else {
      Leaf(Field(), Playing, Root, 0)
    }
  }

  private def buildGameTreeLevel(tree: GameTree, 
      level: Int, currentPlayer: PlayerType): GameTree = {
    Leaf(Field(), Playing, Root, 0)
  }

  def makeMove(field: Field, player: Player) = Move(Cell(0, 0), player)
}
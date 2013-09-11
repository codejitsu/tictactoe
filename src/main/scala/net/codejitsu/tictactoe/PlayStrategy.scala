package net.codejitsu.tictactoe

import scala.util.Random
import scala.annotation.tailrec
import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.GameStatus._
import net.codejitsu.tictactoe.tree.Tree
import net.codejitsu.tictactoe.tree.MoveTree.Step
import net.codejitsu.tictactoe.tree.MoveTree

trait PlayStrategy {
  def makeMove(field: Board, player: Player): Move
}

class RandomMoveStrategy extends PlayStrategy {
  private val rand = new Random()
  def makeMove(field: Board, player: Player) = 
    Move(Cell(this.rand.nextInt(FieldSize), this.rand.nextInt(FieldSize)), player)
}

class ReadConsoleStrategy extends PlayStrategy {
  @tailrec
  private def doMove(field: Board, player: Player): Move = {
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

  def makeMove(field: Board, player: Player): Move = {
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
  private lazy val gameTree: Tree[Step] = MoveTree.build(X)

  def makeMove(field: Board, player: Player) = Move(Cell(0, 0), player)
}
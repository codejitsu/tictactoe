package net.codejitsu.tictactoe

import scala.util.Random
import scala.annotation.tailrec
import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.GameStatus._
import net.codejitsu.tictactoe.tree.Tree
import net.codejitsu.tictactoe.tree.MoveTree.Step
import net.codejitsu.tictactoe.tree.MoveTree

object PlayStrategy {
  type StrategyFun = (Board, Player) => Move

  val random: StrategyFun = (board, player) => {
    val rand = new Random()
    Move(Cell(rand.nextInt(FieldSize), rand.nextInt(FieldSize)), player.playerType)
  }

  val readConsole: StrategyFun = (board, player) => {
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

    def makeMove(board: Board, player: Player): Move = {
      print(player.playerType + ": ")

      val input = readLine()

      println()

      if (!verifyInput(input)) {
        println("Invalid input.")
        makeMove(board, player)
      } else {
        val coordinates = parseCoordinates(input)
        Move(Cell(coordinates._1, coordinates._2), player.playerType)
      }
    }

    makeMove(board, player)
  }
  
  val god: StrategyFun = (board, player) => {
    lazy val gameTree: Tree[Step] = MoveTree.build(PlayerType.X)
    
    Move(Cell(0, 0), player.playerType)
  }
}
package net.codejitsu.tictactoe

import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.CellStatus._

object GameStatus extends Enumeration {
  type GameStatus = Value
  val NotStarted, Playing, XWon, OWon, Tie = Value
}

import GameStatus._

class GameOverException extends Exception

case class Game(val playerX: Player, val playerO: Player) {
  require(playerX != null && playerO != null)
  require(playerX.name != playerO.name)
  require(playerX.playerType != playerO.playerType)

  def getPlayer(ptype: PlayerType): Player = if (ptype == PlayerType.X) this.playerX else playerO

  def makeMove(player: PlayerType, board: Board): Option[Board] = 
    board.update(getPlayer(player).makeMove(board))

  def checkFieldSpace(board: Board, f: Int => Set[Cell]): GameStatus = {
    val space = List(0, 1, 2).map(f(_))

    val checkedSpace = space.map(allCellsOccupiedBySamePlayer _)

    if (checkedSpace.exists(r => r._1 == true)) {
      val wonElement = checkedSpace.find(r => r._1 == true)

      wonElement match {
        case Some(r) => r._2 match {
          case Some(p) => p match {
            case OccupiedByX => XWon
            case OccupiedByO => OWon
          }
          case None => Playing
        }
        case None => Playing
      }
    } else {
      Playing
    }    
  }

  def calculateStatus(board: Board): GameStatus = {
    val horizontalStatus = checkFieldSpace(board, board.getRow)
    val verticalStatus = checkFieldSpace(board, board.getColumn)
    
    if (horizontalStatus != Playing) horizontalStatus
    else if (verticalStatus != Playing) verticalStatus
    else {
    	val diag1Status = allCellsOccupiedBySamePlayer(board.getFirstDiagonal)
    	val diag2Status = allCellsOccupiedBySamePlayer(board.getSecondDiagonal)
    	
    	if (diag1Status._1 == true) {
	      diag1Status._2 match {
	        case Some(r) => r match {
	            case OccupiedByX => XWon
	            case OccupiedByO => OWon
	          }
	        case None => Playing
	      }    	  
    	} else if (diag2Status._1 == true) {
   	      diag2Status._2 match {
	        case Some(r) => r match {
	            case OccupiedByX => XWon
	            case OccupiedByO => OWon
	          }
	          case None => Playing
	      } 	  
    	} else {
    	  if (board.isFull) Tie
    	  else Playing
    	}
    }
  }

  def allCellsOccupiedBySamePlayer(cells: Set[Cell]): (Boolean, Option[CellStatus]) = {
    if (cells.size != FieldSize) {
      (false, None)
    } else {
      val allPlayers = cells.map(c => c.status)
      (allPlayers.size == 1, Option(allPlayers.toList.head))
    }
  }
}
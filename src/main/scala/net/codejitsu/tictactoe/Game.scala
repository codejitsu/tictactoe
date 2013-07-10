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

  def makeMove(player: PlayerType, field: Field): (Field, Boolean) = {
    try {
      (field.update(getPlayer(player).makeMove(field)), true)
    } catch {
      case ise: IllegalStateException => {
        (field, false)
      }
    }
  }

  def checkFieldSpace(field: Field, f: Int => Set[(CellStatus, Int, Int)]): GameStatus = {
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
          case _ => Playing
        }
        case None => Playing
      }
    } else {
      Playing
    }    
  }

  def calculateStatus(field: Field): GameStatus = {
    val horizontalStatus = checkFieldSpace(field, field.getRow)
    val verticalStatus = checkFieldSpace(field, field.getColumn)
    
    if (horizontalStatus != Playing) horizontalStatus
    else if (verticalStatus != Playing) verticalStatus
    else {
    	val diag1Status = allCellsOccupiedBySamePlayer(field.getFirstDiagonal)
    	val diag2Status = allCellsOccupiedBySamePlayer(field.getSecondDiagonal)
    	
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
    	  if (field.isFull) Tie
    	  else Playing
    	}
    }
  }

  def allCellsOccupiedBySamePlayer(cells: Set[(CellStatus, Int, Int)]): (Boolean, Option[CellStatus]) = {
    if (cells.size != FieldSize) {
      (false, None)
    } else {
      val allPlayers = cells.map(c => c._1)
      (allPlayers.size == 1, Option(allPlayers.toList.head))
    }
  }
}
package net.codejitsu.tictactoe

import PlayerType._

object CellStatus extends Enumeration {
  type CellStatus = Value
  val OccupiedByX, OccupiedByO = Value
}
import CellStatus._

case class Field(val field: List[(CellStatus, Int, Int)]) {
  def isEmpty = field.isEmpty

  def isFull = field.size == FieldSize * FieldSize
  
  def update(move: Move): Field = move.player.playerType match {    
    case X if verify(move) => this.copy(field = (OccupiedByX, move.row, move.col) :: field)
    case O if verify(move) => this.copy(field = (OccupiedByO, move.row, move.col) :: field)
  }
  
  def verify(move: Move): Boolean = {
	if (!field.exists(c => c._2 == move.row && c._3 == move.col)) true
	else throw new IllegalStateException
  }
}

object Field {
  def apply() = new Field(List())
}
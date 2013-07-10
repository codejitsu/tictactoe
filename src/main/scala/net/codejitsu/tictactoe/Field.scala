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
    case X if verify(move) => this.copy(field = ((OccupiedByX, move.row, move.col) :: field))
    case O if verify(move) => this.copy(field = ((OccupiedByO, move.row, move.col) :: field))
  }
  
  def verify(move: Move): Boolean = {
	if (!field.exists(c => c._2 == move.row && c._3 == move.col)) true
	else throw new IllegalStateException
  }
  
  def verify(x: Int) = {
    if (x > FieldSize - 1 || x < 0) throw new IllegalArgumentException
  }
  
  def getRow(row: Int): Set[(CellStatus, Int, Int)] = {
    verify(row)
    field.filter(c => c._2 == row).toSet
  }

  def getColumn(col: Int): Set[(CellStatus, Int, Int)] = {
    verify(col)
    field.filter(c => c._3 == col).toSet
  }
  
  def getFirstDiagonal(): Set[(CellStatus, Int, Int)] = {
    field.filter(c => c._2 == c._3).toSet
  }  

  def getSecondDiagonal(): Set[(CellStatus, Int, Int)] = {
    field.filter(c => c._2 == Math.abs(c._3 - FieldSize + 1)).toSet
  }  
}

object Field {
  def apply() = new Field(List())
}
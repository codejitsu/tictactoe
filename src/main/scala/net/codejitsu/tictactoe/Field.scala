package net.codejitsu.tictactoe

import PlayerType._

object CellStatus extends Enumeration {
  type CellStatus = Value
  val OccupiedByX, OccupiedByO = Value
}
import CellStatus._

case class Cell(row: Int, col: Int)

case class Field(val field: List[(CellStatus, Cell)]) {
  def isEmpty = field.isEmpty

  def isFull = field.size == FieldSize * FieldSize

  def update(move: Move): Field = move.player.playerType match {
    case X if verify(move) => this.copy(field = ((OccupiedByX, move.cell) :: field))
    case O if verify(move) => this.copy(field = ((OccupiedByO, move.cell) :: field))
  }

  def verify(move: Move): Boolean = {
    if (!field.exists(c => c._2.row == move.cell.row && c._2.col == move.cell.col)) true
    else throw new IllegalStateException()
  }

  def silentVerify(move: Move): Boolean = silentVerify(move.cell.row, move.cell.col)
    
  def silentVerify(row: Int, col: Int): Boolean = {
    if (!field.exists(c => c._2.row == row && c._2.col == col)) true
    else false    
  }  
  
  def verify(x: Int) = {
    if (x > FieldSize - 1 || x < 0) throw new IllegalArgumentException
  }

  def getRow(row: Int): Set[(CellStatus, Cell)] = {
    verify(row)
    field.filter(c => c._2.row == row).toSet
  }

  def getColumn(col: Int): Set[(CellStatus, Cell)] = {
    verify(col)
    field.filter(c => c._2.col == col).toSet
  }

  def getFirstDiagonal(): Set[(CellStatus, Cell)] = {
    field.filter(c => c._2.row == c._2.col).toSet
  }

  def getSecondDiagonal(): Set[(CellStatus, Cell)] = {
    field.filter(c => c._2.row == Math.abs(c._2.col - FieldSize + 1)).toSet
  }

  override def toString(): String = {
    if (isEmpty) "000\n000\n000\n"
    else {
      val cells =
        for (
          row <- 0 until FieldSize;
          col <- 0 until FieldSize;
          
          eol = if (col == FieldSize - 1) "\n" else ""
        ) yield getCell(row, col) + eol
      
      cells.foldLeft("")(_ + _)
    }
  }

  def getCell(row: Int, col: Int): String = {
    val cell = field.filter(c => c._2.row == row && c._2.col == col)
    
    if (cell.isEmpty) "0"
    else cell(0)._1 match {
      case OccupiedByX => "X"
      case OccupiedByO => "O"
    }
  }

  override def hashCode = 41 + this.toString.hashCode
  override def equals(other: Any): Boolean = other match {
    case that: Field => (
      that.canEqual(this)
      && this.toString == that.toString)
    case _ => false
  }
  def canEqual(other: Any): Boolean = other.isInstanceOf[Field]  
}

object Field {
  def apply() = new Field(List())
}
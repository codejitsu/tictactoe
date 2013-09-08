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
    else throw new IllegalStateException()
  }

  def silentVerify(move: Move): Boolean = silentVerify(move.row, move.col)
    
  def silentVerify(row: Int, col: Int): Boolean = {
    if (!field.exists(c => c._2 == row && c._3 == col)) true
    else false    
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
    val cell = field.filter(c => c._2 == row && c._3 == col)
    
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
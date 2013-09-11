package net.codejitsu.tictactoe

import PlayerType._

object CellStatus extends Enumeration {
  type CellStatus = Value
  val OccupiedByX, OccupiedByO, Free = Value
}
import CellStatus._

/**
 * Cell on the board.
 */
case class Cell(row: Int, col: Int, status: CellStatus = Free)

/**
 * Board to play TicTacToe.
 */
case class Board(val cells: List[Cell]) {
  def isEmpty = cells.isEmpty

  def isFull = cells.size == FieldSize * FieldSize

  def update(move: Move): Board = move.player.playerType match {
    case X if verify(move) => Board.this.copy(cells = move.cell.copy(status = OccupiedByX) :: cells)
    case O if verify(move) => Board.this.copy(cells = move.cell.copy(status = OccupiedByO) :: cells)
  }

  def verify(move: Move): Boolean = {
    if (!cells.exists(c => c.row == move.cell.row && c.col == move.cell.col)) true
    else throw new IllegalStateException()
  }

  def silentVerify(move: Move): Boolean = silentVerify(move.cell.row, move.cell.col)
    
  def silentVerify(row: Int, col: Int): Boolean = {
    if (!cells.exists(c => c.row == row && c.col == col)) true
    else false    
  }  
  
  def verify(x: Int) = {
    if (x > FieldSize - 1 || x < 0) throw new IllegalArgumentException
  }

  def getRow(row: Int): Set[Cell] = {
    verify(row)
    cells.filter(c => c.row == row).toSet
  }

  def getColumn(col: Int): Set[Cell] = {
    verify(col)
    cells.filter(c => c.col == col).toSet
  }

  def getFirstDiagonal(): Set[Cell] = {
    cells.filter(c => c.row == c.col).toSet
  }

  def getSecondDiagonal(): Set[Cell] = {
    cells.filter(c => c.row == Math.abs(c.col - FieldSize + 1)).toSet
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
    val cell = cells.filter(c => c.row == row && c.col == col)
    
    if (cell.isEmpty) "0"
    else cell(0).status match {
      case OccupiedByX => "X"
      case OccupiedByO => "O"
      case Free => "0"
    }
  }

  override def hashCode = 41 + Board.this.toString.hashCode
  override def equals(other: Any): Boolean = other match {
    case that: Board => (
      that.canEqual(Board.this)
      && Board.this.toString == that.toString)
    case _ => false
  }
  def canEqual(other: Any): Boolean = other.isInstanceOf[Board]  
}

object Board {
  def apply() = new Board(List())
}
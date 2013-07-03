package net.codejitsu.tictactoe

import PlayerType._

object CellStatus extends Enumeration {
  type CellStatus = Value
  val OccupiedByX, OccupiedByO = Value
}
import CellStatus._

case class Field(val field: List[(CellStatus, Int, Int)]) {
  def isEmpty = field.isEmpty

  def update(move: Move): Field = move.player.playerType match {    
    case X => this.copy(field = (OccupiedByX, move.row, move.col) :: field)
    case O => this.copy(field = (OccupiedByO, move.row, move.col) :: field)
  }
}

object Field {
  def apply() = new Field(List())
}
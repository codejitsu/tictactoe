package net.codejitsu.tictactoe

object CellStatus extends Enumeration {
  type CellStatus = Value
  val OccupiedByX, OccupiedByO, EmptyCell = Value
}
import CellStatus._

case class Field(val field: Seq[CellStatus]) {
  def isEmpty = field.forall(_ == EmptyCell)

  def update(move: Move): Field = {
    this.copy(field = field.updated(2, OccupiedByX))
  }
}

object Field {
  def apply() = new Field(List(EmptyCell, EmptyCell, EmptyCell,
    EmptyCell, EmptyCell, EmptyCell,
    EmptyCell, EmptyCell, EmptyCell))
}
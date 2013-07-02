package net.codejitsu.tictactoe

object CellStatus extends Enumeration {
  type CellStatus = Value
  val OccupiedByX, OccupiedByO, EmptyCell = Value
}
import CellStatus._

class Field {
	private val field: Seq[CellStatus] = 
	  List(EmptyCell, EmptyCell, EmptyCell, 
	       EmptyCell, EmptyCell, EmptyCell, 
	       EmptyCell, EmptyCell, EmptyCell)
	
	def isEmpty = field.forall(_ == EmptyCell)
}

object Field {
  def apply() = new Field
}
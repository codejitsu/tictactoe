package net.codejitsu.tictactoe

class Move(val row: Int, val col: Int) {
	require(row >= 0 && row < FieldSize)
	require(col >= 0 && row < FieldSize)
}

object Move {
  def apply(row: Int, col: Int) = new Move(row, col)
}
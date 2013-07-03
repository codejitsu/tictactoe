package net.codejitsu.tictactoe

case class Move(val row: Int, val col: Int, val player: Player) {
	require(row >= 0 && row < FieldSize)
	require(col >= 0 && row < FieldSize)
}
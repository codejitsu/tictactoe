package net.codejitsu.tictactoe

case class Move(val cell: Cell, val player: Player) {
	require(cell.row >= 0 && cell.row < FieldSize)
	require(cell.col >= 0 && cell.row < FieldSize)
}
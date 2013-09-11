package net.codejitsu.tictactoe

import net.codejitsu.tictactoe.PlayerType._

case class Move(val cell: Cell, val player: PlayerType) {
	require(cell.row >= 0 && cell.row < FieldSize)
	require(cell.col >= 0 && cell.row < FieldSize)
}
package net.codejitsu.tictactoe

import scala.util.Random

trait PlayStrategy {
	def makeMove(field: Field): Move
}

class RandomMoveStrategy extends PlayStrategy {
    private val rand = new Random()
	def makeMove(field: Field): Move = Move(this.rand.nextInt(FieldSize), this.rand.nextInt(FieldSize))
}
package net.codejitsu.tictactoe

import scala.util.Random

class RandomMoveStrategy extends PlayStrategy {
    private val rand = new Random()
	def makeMove(): Move = Move(this.rand.nextInt(FieldSize), this.rand.nextInt(FieldSize))
}
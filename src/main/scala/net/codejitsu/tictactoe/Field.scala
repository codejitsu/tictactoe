package net.codejitsu.tictactoe

import scala.util.Either
import net.codejitsu.tictactoe.PlayerType._

class Field {
	private val field: Seq[Either[PlayerType, Null]] = List(null, null, null, null, null, null, null, null, null)
}

object Field {
  def apply() = new Field
}
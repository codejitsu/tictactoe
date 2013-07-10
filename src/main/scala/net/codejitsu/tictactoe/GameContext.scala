package net.codejitsu.tictactoe

import scala.annotation.tailrec

import GameStatus._
import net.codejitsu.tictactoe.PlayerType._

case class GameContext(val playerX: Player, val playerO: Player, val currentPlayer: PlayerType, val status: GameStatus) {
  val game = Game(playerX, playerO)

  def nextPlayer = currentPlayer match {
    case X => O
    case O => X
  }

  @tailrec
  private def safeMove(field: Field): Field = {
    game.makeMove(currentPlayer, field) match {
      case (_, false) => safeMove(field)
      case (f, true) => f
    }
  }

  def start(): (Field, GameContext) = {
    if (status != NotStarted) throw new IllegalStateException

    move(Field(), this.copy(status = Playing, currentPlayer = currentPlayer))
  }

  def move(field: Field, context: GameContext): (Field, GameContext) = {
    if (context.status == GameStatus.OWon || context.status == GameStatus.XWon || context.status == GameStatus.Tie) {
      throw new IllegalStateException
    }

    val gameStatus = game.calculateStatus(field)

    if (gameStatus != GameStatus.Playing) {
      (field, context.copy(status = gameStatus))
    } else {
      try {
        triggerMove(field, context)
      } catch {
        case goe: GameOverException => {
          (field, context.copy(status = game.calculateStatus(field)))
        }
      }
    }
  }

  private def triggerMove(field: Field, context: GameContext): (Field, GameContext) = {
    if (context.status != Playing) throw new IllegalStateException
    if (field.isFull) throw new GameOverException

    (safeMove(field),
      context.copy(status = Playing, currentPlayer = context.nextPlayer))
  }
}
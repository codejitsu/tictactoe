package net.codejitsu.tictactoe

import scala.annotation.tailrec

import GameStatus._
import net.codejitsu.tictactoe.PlayerType._

case class GameContext(val playerX: Player, val playerO: Player, val currentPlayer: PlayerType, val status: GameStatus,
    onError: Unit => Unit) {
  val game = Game(playerX, playerO)

  def nextPlayer = currentPlayer match {
    case X => O
    case O => X
  }

  @tailrec
  private def safeMove(field: Board): Board = {
    game.makeMove(currentPlayer, field) match {
      case (_, false) => {
        onError()
        safeMove(field)
      }
      case (f, true) => f
    }
  }

  def start(): (Board, GameContext) = {
    if (status != NotStarted) throw new IllegalStateException

    this.copy(status = Playing, currentPlayer = currentPlayer).move(Board())
  }

  def move(field: Board): (Board, GameContext) = {
    if (this.status == GameStatus.OWon || this.status == GameStatus.XWon || this.status == GameStatus.Tie) {
      throw new IllegalStateException
    }

    val gameStatus = game.calculateStatus(field)

    if (gameStatus != GameStatus.Playing) {
      (field, this.copy(status = gameStatus))
    } else {
      try {
        triggerMove(field, this)
      } catch {
        case goe: GameOverException => {
          (field, this.copy(status = game.calculateStatus(field)))
        }
      }
    }
  }

  private def triggerMove(field: Board, context: GameContext): (Board, GameContext) = {
    if (context.status != Playing) throw new IllegalStateException
    if (field.isFull) throw new GameOverException

    (safeMove(field),
      context.copy(status = Playing, currentPlayer = context.nextPlayer))
  }
  
  def statusString: String = status match {
    case NotStarted => "Not started yet."
    case Playing => "Playing"
    case XWon => "X won!" 
    case OWon => "O won!" 
    case Tie => "Tie!"
  }
}
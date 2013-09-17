package net.codejitsu.tictactoe

import scala.annotation.tailrec
import GameStatus._
import net.codejitsu.tictactoe.PlayerType._
import scala.util.Try
import scala.util.Failure
import scala.util.Success

case class GameContext(val playerX: Player, val playerO: Player, val currentPlayer: PlayerType, val status: GameStatus,
    onError: Unit => Unit) {
  val game = Game(playerX, playerO)

  def nextPlayer = currentPlayer match {
    case X => O
    case O => X
  }

  @tailrec
  private def safeMove(board: Board): Board = game.makeMove(currentPlayer, board) match {
    case None => {
      onError()
      safeMove(board)
    }
    case Some(f) => f
  }

  def start: Try[(Board, GameContext)] = {
    if (status != NotStarted) Failure(new IllegalStateException)
    else this.copy(status = Playing, currentPlayer = currentPlayer).move(Board())
  }

  def move(board: Board): Try[(Board, GameContext)] = {
    if (this.status == OWon || this.status == XWon || this.status == Tie) Failure(new IllegalStateException)
    else {
      val gameStatus = game.calculateStatus(board)

      if (gameStatus != Playing) {
        Success((board, this.copy(status = gameStatus)))
      } else {
        try {
          triggerMove(board, this)
        } catch {
          case goe: GameOverException => {
            Success((board, this.copy(status = game.calculateStatus(board))))
          }
        }
      }
    }
  }

  private def triggerMove(board: Board, context: GameContext): Try[(Board, GameContext)] = {
    if (context.status != Playing) Failure(new IllegalStateException)
    else if (board.isFull) Failure(new GameOverException)
    else Success((safeMove(board),
      context.copy(status = Playing, currentPlayer = context.nextPlayer)))
  }
  
  def statusString: String = status match {
    case NotStarted => "Not started yet."
    case Playing => "Playing"
    case XWon => "X won!" 
    case OWon => "O won!" 
    case Tie => "Tie!"
  }
}
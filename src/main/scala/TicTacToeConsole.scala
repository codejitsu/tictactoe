import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.RandomMoveStrategy
import net.codejitsu.tictactoe.GameContext
import net.codejitsu.tictactoe.GameStatus._
import net.codejitsu.tictactoe.ReadConsoleStrategy
import net.codejitsu.tictactoe.Board
import scala.util.Failure
import scala.util.Success

object TicTacToeConsole extends App {
  val playerOne = Player("Player 1", X, new ReadConsoleStrategy())
  val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

  val initContext = GameContext(playerOne, playerTwo, X, NotStarted,
    _ => println("Error: Illegal move!\n")).start

  def play(board: Board, context: GameContext): Unit = {
    if (context.currentPlayer == playerOne.playerType) println(board.toString)

    if (context.status == Playing) {
      val step = context.move(board)
      step match {
        case Success(c) => play(c._1, c._2)
        case Failure(_) => println("Error!")
      }
    } else {
      println(board.toString)
      println("Game over: " + context.statusString)
    }
  }

  initContext match {
    case Failure(_) => println("Game initialization error.")
    case Success(c) => play(c._1, c._2)
  }
}
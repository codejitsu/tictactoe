import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.RandomMoveStrategy
import net.codejitsu.tictactoe.GameContext
import net.codejitsu.tictactoe.GameStatus._
import net.codejitsu.tictactoe.ReadConsoleStrategy
import net.codejitsu.tictactoe.Field

object TicTacToeConsole extends App {
  val playerOne = Player("Player 1", X, new ReadConsoleStrategy())
  val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

  val initContext = GameContext(playerOne, playerTwo, X, NotStarted, 
      _ => println("Error: Illegal move!\n")).start()

  def contexts(field: Field, context: GameContext): Unit = {
    if (context.currentPlayer == playerOne.playerType) println(field.toString)

    if (context.status == Playing) {
      val step = context.move(field)
      contexts(step._1, step._2)
    } else {
      println("Game over: " + context.statusString)
    }
  }

  contexts(initContext._1, initContext._2)
}
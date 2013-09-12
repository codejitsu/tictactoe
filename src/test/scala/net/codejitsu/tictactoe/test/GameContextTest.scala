package net.codejitsu.tictactoe.test

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Test

import net.codejitsu.tictactoe.Board
import net.codejitsu.tictactoe.Cell
import net.codejitsu.tictactoe.GameContext
import net.codejitsu.tictactoe.GameStatus.NotStarted
import net.codejitsu.tictactoe.GameStatus.Playing
import net.codejitsu.tictactoe.GameStatus.Tie
import net.codejitsu.tictactoe.GameStatus.XWon
import net.codejitsu.tictactoe.Move
import net.codejitsu.tictactoe.PlayStrategy
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType.O
import net.codejitsu.tictactoe.PlayerType.PlayerType
import net.codejitsu.tictactoe.PlayerType.X
import net.codejitsu.tictactoe.RandomMoveStrategy

class GameControllerTest {
  @Test def initGameController() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ())

    assertEquals(NotStarted, context.status)
    assertEquals(O, context.nextPlayer)

    val afterStart = context.start

    assertNotNull(afterStart._2.game)
    assertEquals(Playing, afterStart._2.status)
  }

  @Test(expected = classOf[IllegalStateException])
  def startCalledTwiceResultsInExceptions() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ())

    assertEquals(NotStarted, context.status)

    val afterStart = context.start
    afterStart._2.start
  }

  @Test
  def atTheEndTheFieldIsFullIfTie() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start

    val board = Board()
    val board1 = board.update(Move(Cell(0, 0), X))
    val board2 = board1.update(Move(Cell(0, 2), O))
    val board3 = board2.update(Move(Cell(0, 1), X))
    val board4 = board3.update(Move(Cell(1, 0), O))
    val board5 = board4.update(Move(Cell(1, 2), X))
    val board6 = board5.update(Move(Cell(1, 1), O))
    val board7 = board6.update(Move(Cell(2, 0), X))
    val board8 = board7.update(Move(Cell(2, 2), O))
    val board9 = board8.update(Move(Cell(2, 1), X))

    val tie = context._2.move(board9)

    assertTrue(tie._1.isFull)
    assertTrue(tie._2.status == Tie)
  }

  @Test(expected = classOf[ArithmeticException])
  def onErrorCalled() {
    val playerOne = Player("Player 1", X, new PlayStrategy {
      def makeMove(field: Board, player: Player): Move = Move(Cell(0, 0), player.playerType)
    })
    
    val playerTwo = Player("Player 2", O, new PlayStrategy {
      def makeMove(field: Board, player: Player): Move = Move(Cell(0, 0), player.playerType)
    })

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => throw new ArithmeticException).start
    context._2.move(context._1)
  }  
  
  @Test
  def playersSwitchedCorrectly() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ())

    assertEquals(X, context.currentPlayer)

    try {
      val step1 = context.start
      checkCurrentPlayer(step1._2, O)

      val step2 = step1._2.move(step1._1)
      checkCurrentPlayer(step2._2, X)

      val step3 = step2._2.move(step2._1)
      checkCurrentPlayer(step3._2, O)

      val step4 = step3._2.move(step3._1)
      checkCurrentPlayer(step4._2, X)

      val step5 = step4._2.move(step4._1)
      checkCurrentPlayer(step5._2, O)

      val step6 = step5._2.move(step5._1)
      checkCurrentPlayer(step6._2, X)

      val step7 = step6._2.move(step6._1)
      checkCurrentPlayer(step7._2, O)

      val step8 = step7._2.move(step7._1)
      checkCurrentPlayer(step8._2, X)

      val step9 = step8._2.move(step8._1)
      checkCurrentPlayer(step9._2, O)
    } catch {
      case is: IllegalStateException =>
    }
  }

  def checkCurrentPlayer(context: GameContext, expectedPlayer: PlayerType) =
    if (context.status == Playing) assertEquals(expectedPlayer, context.currentPlayer)

  @Test
  def playerXWonHorizontally() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start

    val board = Board()

    val board1 = board.update(Move(Cell(0, 0), playerOne.playerType))

    val board2 = board1.update(Move(Cell(1, 1), playerTwo.playerType))

    val board3 = board2.update(Move(Cell(0, 1), playerOne.playerType))

    val board4 = board3.update(Move(Cell(2, 2), playerTwo.playerType))

    val board5 = board4.update(Move(Cell(0, 2), playerOne.playerType))

    val gameOverXWon = context._2.move(board5)

    assertEquals(XWon, gameOverXWon._2.status)
  }

  @Test
  def playerXWonVertically() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start

    val board = Board()
    val board1 = board.update(Move(Cell(0, 0), playerOne.playerType))
    val board2 = board1.update(Move(Cell(1, 1), playerTwo.playerType))
    val board3 = board2.update(Move(Cell(1, 0), playerOne.playerType))
    val board4 = board3.update(Move(Cell(2, 2), playerTwo.playerType))
    val board5 = board4.update(Move(Cell(2, 0), playerOne.playerType))

    val gameOverXWon = context._2.move(board5)

    assertEquals(XWon, gameOverXWon._2.status)
  }

  @Test
  def playerXWonDiagonal1() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start

    val board = Board()
    val board1 = board.update(Move(Cell(0, 0), playerOne.playerType))
    val board2 = board1.update(Move(Cell(1, 0), playerTwo.playerType))
    val board3 = board2.update(Move(Cell(1, 1), playerOne.playerType))
    val board4 = board3.update(Move(Cell(0, 1), playerTwo.playerType))
    val board5 = board4.update(Move(Cell(2, 2), playerOne.playerType))

    val gameOverXWon = context._2.move(board5)

    assertEquals(XWon, gameOverXWon._2.status)
  }

  @Test
  def playerXWonDiagonal2() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start

    val board = Board()
    val board1 = board.update(Move(Cell(0, 2), playerOne.playerType))
    val board2 = board1.update(Move(Cell(1, 0), playerTwo.playerType))
    val board3 = board2.update(Move(Cell(1, 1), playerOne.playerType))
    val board4 = board3.update(Move(Cell(0, 1), playerTwo.playerType))
    val board5 = board4.update(Move(Cell(2, 0), playerOne.playerType))

    val gameOverXWon = context._2.move(board5)

    assertEquals(XWon, gameOverXWon._2.status)
  }

  @Test
  def tie() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start

    val board = Board()
    val board1 = board.update(Move(Cell(0, 0), playerOne.playerType))
    val board2 = board1.update(Move(Cell(0, 2), playerTwo.playerType))
    val board3 = board2.update(Move(Cell(0, 1), playerOne.playerType))
    val board4 = board3.update(Move(Cell(1, 0), playerTwo.playerType))
    val board5 = board4.update(Move(Cell(1, 2), playerOne.playerType))
    val board6 = board5.update(Move(Cell(1, 1), playerTwo.playerType))
    val board7 = board6.update(Move(Cell(2, 0), playerOne.playerType))
    val board8 = board7.update(Move(Cell(2, 2), playerTwo.playerType))
    val board9 = board8.update(Move(Cell(2, 1), playerOne.playerType))

    val tie = context._2.move(board9)

    assertEquals(Tie, tie._2.status)
  }
}
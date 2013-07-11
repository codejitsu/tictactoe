package net.codejitsu.tictactoe.test

import org.junit.Test
import org.junit.Assert._
import net.codejitsu.tictactoe.Player
import net.codejitsu.tictactoe.PlayerType._
import net.codejitsu.tictactoe.RandomMoveStrategy
import net.codejitsu.tictactoe.GameContext
import net.codejitsu.tictactoe.GameStatus._
import net.codejitsu.tictactoe.Field
import net.codejitsu.tictactoe.Move
import net.codejitsu.tictactoe.GameContext
import net.codejitsu.tictactoe.PlayStrategy

class GameControllerTest {
  @Test def initGameController() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ())

    assertEquals(NotStarted, context.status)
    assertEquals(O, context.nextPlayer)

    val afterStart = context.start()

    assertNotNull(afterStart._2.game)
    assertEquals(Playing, afterStart._2.status)
  }

  @Test(expected = classOf[IllegalStateException])
  def startCalledTwiceResultsInExceptions() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ())

    assertEquals(NotStarted, context.status)

    val afterStart = context.start()
    afterStart._2.start()
  }

  @Test
  def atTheEndTheFieldIsFullIfTie() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start()

    val field = Field()
    val field1 = field.update(Move(0, 0, playerOne))
    val field2 = field1.update(Move(0, 2, playerTwo))
    val field3 = field2.update(Move(0, 1, playerOne))
    val field4 = field3.update(Move(1, 0, playerTwo))
    val field5 = field4.update(Move(1, 2, playerOne))
    val field6 = field5.update(Move(1, 1, playerTwo))
    val field7 = field6.update(Move(2, 0, playerOne))
    val field8 = field7.update(Move(2, 2, playerTwo))
    val field9 = field8.update(Move(2, 1, playerOne))

    val tie = context._2.move(field9)

    assertTrue(tie._1.isFull)
    assertTrue(tie._2.status == Tie)
  }

  @Test(expected = classOf[ArithmeticException])
  def onErrorCalled() {
    val playerOne = Player("Player 1", X, new PlayStrategy {
      def makeMove(field: Field, player: Player): Move = Move(0, 0, player)
    })
    
    val playerTwo = Player("Player 2", O, new PlayStrategy {
      def makeMove(field: Field, player: Player): Move = Move(0, 0, player)
    })

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => throw new ArithmeticException).start()
    context._2.move(context._1)
  }  
  
  @Test
  def playersSwitchedCorrectly() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ())

    assertEquals(X, context.currentPlayer)

    try {
      val step1 = context.start()
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

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start()

    val field = Field()

    val field1 = field.update(Move(0, 0, playerOne))

    val field2 = field1.update(Move(1, 1, playerTwo))

    val field3 = field2.update(Move(0, 1, playerOne))

    val field4 = field3.update(Move(2, 2, playerTwo))

    val field5 = field4.update(Move(0, 2, playerOne))

    val gameOverXWon = context._2.move(field5)

    assertEquals(XWon, gameOverXWon._2.status)
  }

  @Test
  def playerXWonVertically() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start()

    val field = Field()
    val field1 = field.update(Move(0, 0, playerOne))
    val field2 = field1.update(Move(1, 1, playerTwo))
    val field3 = field2.update(Move(1, 0, playerOne))
    val field4 = field3.update(Move(2, 2, playerTwo))
    val field5 = field4.update(Move(2, 0, playerOne))

    val gameOverXWon = context._2.move(field5)

    assertEquals(XWon, gameOverXWon._2.status)
  }

  @Test
  def playerXWonDiagonal1() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start()

    val field = Field()
    val field1 = field.update(Move(0, 0, playerOne))
    val field2 = field1.update(Move(1, 0, playerTwo))
    val field3 = field2.update(Move(1, 1, playerOne))
    val field4 = field3.update(Move(0, 1, playerTwo))
    val field5 = field4.update(Move(2, 2, playerOne))

    val gameOverXWon = context._2.move(field5)

    assertEquals(XWon, gameOverXWon._2.status)
  }

  @Test
  def playerXWonDiagonal2() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start()

    val field = Field()
    val field1 = field.update(Move(0, 2, playerOne))
    val field2 = field1.update(Move(1, 0, playerTwo))
    val field3 = field2.update(Move(1, 1, playerOne))
    val field4 = field3.update(Move(0, 1, playerTwo))
    val field5 = field4.update(Move(2, 0, playerOne))

    val gameOverXWon = context._2.move(field5)

    assertEquals(XWon, gameOverXWon._2.status)
  }

  @Test
  def tie() {
    val playerOne = Player("Player 1", X, new RandomMoveStrategy())
    val playerTwo = Player("Player 2", O, new RandomMoveStrategy())

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start()

    val field = Field()
    val field1 = field.update(Move(0, 0, playerOne))
    val field2 = field1.update(Move(0, 2, playerTwo))
    val field3 = field2.update(Move(0, 1, playerOne))
    val field4 = field3.update(Move(1, 0, playerTwo))
    val field5 = field4.update(Move(1, 2, playerOne))
    val field6 = field5.update(Move(1, 1, playerTwo))
    val field7 = field6.update(Move(2, 0, playerOne))
    val field8 = field7.update(Move(2, 2, playerTwo))
    val field9 = field8.update(Move(2, 1, playerOne))

    val tie = context._2.move(field9)

    assertEquals(Tie, tie._2.status)
  }
}
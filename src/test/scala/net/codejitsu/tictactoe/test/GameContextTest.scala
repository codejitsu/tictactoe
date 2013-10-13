package net.codejitsu.tictactoe.test

import org.junit.Assert.{assertEquals, assertNotNull, assertTrue, fail}
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
import net.codejitsu.tictactoe.PlayStrategy._
import scala.util.Success
import scala.util.Failure
import net.codejitsu.tictactoe.GameOverException

class GameControllerTest {
  @Test def initGameController() {
    val playerOne = Player("Player 1", X, random)
    val playerTwo = Player("Player 2", O, random)

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ())

    assertEquals(NotStarted, context.status)
    assertEquals(O, context.nextPlayer)

    val afterStart = context.start

    afterStart match {
      case Success(c) => {
        assertNotNull(c._2.game)
        assertEquals(Playing, c._2.status)
      }
      
      case _ => fail()
    }
  }

  @Test
  def startCalledTwiceResultsInExceptions() {
    val playerOne = Player("Player 1", X, random)
    val playerTwo = Player("Player 2", O, random)

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ())

    assertEquals(NotStarted, context.status)

    val afterStart = context.start
    
    val c = afterStart.flatMap(f => f._2.start)
    
    c match {
      case Success(_) => fail()
      case Failure(_) => 
    }
  }

  @Test
  def atTheEndTheFieldIsFullIfTie() {
    val playerOne = Player("Player 1", X, random)
    val playerTwo = Player("Player 2", O, random)

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start

    val board = Board()
    val board1 = board.update(Move(Cell(0, 0), X))
    val board2 = board1.get.update(Move(Cell(0, 2), O))
    val board3 = board2.get.update(Move(Cell(0, 1), X))
    val board4 = board3.get.update(Move(Cell(1, 0), O))
    val board5 = board4.get.update(Move(Cell(1, 2), X))
    val board6 = board5.get.update(Move(Cell(1, 1), O))
    val board7 = board6.get.update(Move(Cell(2, 0), X))
    val board8 = board7.get.update(Move(Cell(2, 2), O))
    val board9 = board8.get.update(Move(Cell(2, 1), X))

    context match {
      case Failure(_) => fail()
      case Success(c) => {
        val tie = c._2.move(board9.get)
        tie match {
          case Success(t) => {
            assertTrue(t._1.isFull)
            assertTrue(t._2.status == Tie)
          }
          case _ => fail()
        }
      }
    }
  }

  @Test(expected = classOf[ArithmeticException])
  def onErrorCalled() {
    val strategy: PlayStrategy.StrategyFun = (board, player) => {
     Move(Cell(0, 0), player.playerType) 
    }
    
    val playerOne = Player("Player 1", X, strategy)
    
    val playerTwo = Player("Player 2", O, strategy)

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => throw new ArithmeticException).start
    
    context match {
      case Failure(_) => fail()
      case Success(c) => c._2.move(c._1)
    }
  }  
  
  @Test
  def playersSwitchedCorrectly() {
    val playerOne = Player("Player 1", X, random)
    val playerTwo = Player("Player 2", O, random)

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ())

    assertEquals(X, context.currentPlayer)

    try {
      val step1 = context.start
      
      step1 match {
        case Success(c) => checkCurrentPlayer(c._2, O)
        case Failure(_) => fail()
      }
      
      val step2 = step1.flatMap(f => f._2.move(f._1))

      step2 match {
        case Success(c) => checkCurrentPlayer(c._2, X)
        case Failure(_) => fail()
      }
      
      val step3 = step2.flatMap(f => f._2.move(f._1))
      
      step3 match {
        case Success(c) => checkCurrentPlayer(c._2, O)
        case Failure(_) => fail()
      }      
      
      val step4 = step3.flatMap(f => f._2.move(f._1))
      
      step4 match {
        case Success(c) => checkCurrentPlayer(c._2, X)
        case Failure(_) => fail()
      }    

      val step5 = step4.flatMap(f => f._2.move(f._1))
      
      step5 match {
        case Success(c) => checkCurrentPlayer(c._2, O)
        case Failure(_) => fail()
      }

      val step6 = step5.flatMap(f => f._2.move(f._1))
      
      step6 match {
        case Success(c) => checkCurrentPlayer(c._2, X)
        case Failure(g: GameOverException) => println("game over")
        case Failure(_) => fail()
      }

      val step7 = step6.flatMap(f => f._2.move(f._1))
      
      step7 match {
        case Success(c) => checkCurrentPlayer(c._2, O)
        case Failure(g: GameOverException) => println("game over")
        case Failure(_) => fail()
      }

      val step8 = step7.flatMap(f => f._2.move(f._1))
      
      step8 match {
        case Success(c) => checkCurrentPlayer(c._2, X)
        case Failure(g: GameOverException) => println("game over")
        case Failure(_) => fail()
      }

      val step9 = step8.flatMap(f => f._2.move(f._1))
      
      step9 match {
        case Success(c) => checkCurrentPlayer(c._2, O)
        case Failure(g: GameOverException) => println("game over")
        case Failure(_) => fail()
      }
    } catch {
      case is: IllegalStateException =>
    }
  }

  def checkCurrentPlayer(context: GameContext, expectedPlayer: PlayerType) =
    if (context.status == Playing) assertEquals(expectedPlayer, context.currentPlayer)

  @Test
  def playerXWonHorizontally() {
    val playerOne = Player("Player 1", X, random)
    val playerTwo = Player("Player 2", O, random)

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start

    val board = Board()

    val board1 = board.update(Move(Cell(0, 0), playerOne.playerType))

    val board2 = board1.get.update(Move(Cell(1, 1), playerTwo.playerType))

    val board3 = board2.get.update(Move(Cell(0, 1), playerOne.playerType))

    val board4 = board3.get.update(Move(Cell(2, 2), playerTwo.playerType))

    val board5 = board4.get.update(Move(Cell(0, 2), playerOne.playerType))

    val gameOverXWon = context.flatMap(f => f._2.move(board5.get))

    gameOverXWon match {
      case Success(c) => assertEquals(XWon, c._2.status)
      case Failure(_) => fail()
    }
  }

  @Test
  def playerXWonVertically() {
    val playerOne = Player("Player 1", X, random)
    val playerTwo = Player("Player 2", O, random)

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start

    val board = Board()
    val board1 = board.update(Move(Cell(0, 0), playerOne.playerType))
    val board2 = board1.get.update(Move(Cell(1, 1), playerTwo.playerType))
    val board3 = board2.get.update(Move(Cell(1, 0), playerOne.playerType))
    val board4 = board3.get.update(Move(Cell(2, 2), playerTwo.playerType))
    val board5 = board4.get.update(Move(Cell(2, 0), playerOne.playerType))

    val gameOverXWon = context.flatMap(f => f._2.move(board5.get))

    gameOverXWon match {
      case Success(c) => assertEquals(XWon, c._2.status)
      case Failure(_) => fail()
    }
  }

  @Test
  def playerXWonDiagonal1() {
    val playerOne = Player("Player 1", X, random)
    val playerTwo = Player("Player 2", O, random)

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start

    val board = Board()
    val board1 = board.update(Move(Cell(0, 0), playerOne.playerType))
    val board2 = board1.get.update(Move(Cell(1, 0), playerTwo.playerType))
    val board3 = board2.get.update(Move(Cell(1, 1), playerOne.playerType))
    val board4 = board3.get.update(Move(Cell(0, 1), playerTwo.playerType))
    val board5 = board4.get.update(Move(Cell(2, 2), playerOne.playerType))

    val gameOverXWon = context.flatMap(f => f._2.move(board5.get))

    gameOverXWon match {
      case Success(c) => assertEquals(XWon, c._2.status)
      case Failure(_) => fail()
    }
  }

  @Test
  def playerXWonDiagonal2() {
    val playerOne = Player("Player 1", X, random)
    val playerTwo = Player("Player 2", O, random)

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start

    val board = Board()
    val board1 = board.update(Move(Cell(0, 2), playerOne.playerType))
    val board2 = board1.get.update(Move(Cell(1, 0), playerTwo.playerType))
    val board3 = board2.get.update(Move(Cell(1, 1), playerOne.playerType))
    val board4 = board3.get.update(Move(Cell(0, 1), playerTwo.playerType))
    val board5 = board4.get.update(Move(Cell(2, 0), playerOne.playerType))

    val gameOverXWon = context.flatMap(f => f._2.move(board5.get))

    gameOverXWon match {
      case Success(c) => assertEquals(XWon, c._2.status)
      case Failure(_) => fail()
    }
  }

  @Test
  def tie() {
    val playerOne = Player("Player 1", X, random)
    val playerTwo = Player("Player 2", O, random)

    val context = GameContext(playerOne, playerTwo, X, NotStarted, _ => ()).start

    val board = Board()
    val board1 = board.update(Move(Cell(0, 0), playerOne.playerType))
    val board2 = board1.get.update(Move(Cell(0, 2), playerTwo.playerType))
    val board3 = board2.get.update(Move(Cell(0, 1), playerOne.playerType))
    val board4 = board3.get.update(Move(Cell(1, 0), playerTwo.playerType))
    val board5 = board4.get.update(Move(Cell(1, 2), playerOne.playerType))
    val board6 = board5.get.update(Move(Cell(1, 1), playerTwo.playerType))
    val board7 = board6.get.update(Move(Cell(2, 0), playerOne.playerType))
    val board8 = board7.get.update(Move(Cell(2, 2), playerTwo.playerType))
    val board9 = board8.get.update(Move(Cell(2, 1), playerOne.playerType))

    val tie = context.flatMap(f => f._2.move(board9.get))

    tie match {
      case Success(c) => assertEquals(Tie, c._2.status)
      case Failure(_) => fail()
    }
  }
}
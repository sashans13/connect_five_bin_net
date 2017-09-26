package game


import scala.collection.mutable
import scala.util.Random

object Game {
    // Start from top, clockwise
    // p is curr pos
    //    7 0 1
    //    6 p 2
    //    5 4 3

    val gameDirH: List[Int] = List[Int](-1, -1, 0, 1, 1, 1, 0, -1)
    val gameDirW: List[Int] = List[Int](0, 1, 1, 1, 0, -1, -1, -1)

    val horizontalDir: (Int, Int) = (2, 6)
    val verticalDir: (Int, Int) = (0, 4)
    val diagSlashDir: (Int, Int) = (1, 5)
    val diagBackslashDir: (Int, Int) = (3, 7)
    val dirGroupsCnt = 4

    val dirs: List[(Int, Int)] = List[(Int, Int)](horizontalDir, verticalDir, diagSlashDir, diagBackslashDir)
}

class Game(startingMoves: Int, numStartingMoves: Int, dimH: Int, dimW: Int) {

    // Check if we can actually play enough starting moves, worst case scenario (all 0s or all 1s)
    if(dimW * ((dimH - 1) / 2) < numStartingMoves) {
        // TODO: this calculation could also be precise by counting 0s and 1s first
        throw new Exception(
            "Can't play this game..." +
            "\ndimH: " + dimH +
            "\ndimW: " + dimW +
            "\nnumStartingMoves: " + numStartingMoves
        )
    }

    private val gameMap: mutable.Map[(Int, Int), GamePlayer] = mutable.Map()

    private var winner: GameWinner = _

    def getWinner: GameWinner = {
        if (this.winner != null) {
            return this.winner
        }

        // =========== Play the game

        // First half:
        // h: [0, dimH / 2)
        // w: [0, dimW)
        // Second half:
        // h: [dimH / 2, dimH)
        // w: [0, dimW)
        val firstHalfH = dimH / 2

        val rand: Random = new Random

        // Note: GamePlayerX always starts first

        // Play first numStartingMoves moves
        for (i <- 0 until numStartingMoves) {
            val playerOnMove = if ((i & 1) == 0) GamePlayerX else GamePlayerO

            var playPosH: Int = -1
            var playPosW: Int = -1

            do {
                // Which half to play?
                val half = (startingMoves >> i) & 1
                half match {
                    case 0 =>
                        playPosH = rand.nextInt(firstHalfH)
                    case 1 =>
                        playPosH = rand.nextInt(dimH - firstHalfH) + firstHalfH
                }
                playPosW = rand.nextInt(dimW)

                // Keep going until that position already exists
                // NOTE: This can end up in an infinite loop
            } while (this.gameMap.contains((playPosH, playPosW)))

            // Play the move
            this.gameMap((playPosH, playPosW)) = playerOnMove

            val winnerOpt: Option[GamePlayer] = checkIfWon(playPosH, playPosW)
            if (winnerOpt.isDefined) {
                this.winner = winnerOpt.get match {
                    case GamePlayerX => GameWinnerX
                    case GamePlayerO => GameWinnerO
                }

                return this.winner
            }
        }

        // Play the rest of the moves
        while (this.gameMap.size < dimH * dimW) {
            val playerOnMove = if ((this.gameMap.size & 1) == 0) GamePlayerX else GamePlayerO

            var playPosH: Int = -1
            var playPosW: Int = -1

            do {
                playPosH = rand.nextInt(dimH)
                playPosW = rand.nextInt(dimW)
            } while (this.gameMap.contains((playPosH, playPosW)))

            // Play the move
            this.gameMap((playPosH, playPosW)) = playerOnMove

            val winnerOpt: Option[GamePlayer] = checkIfWon(playPosH, playPosW)
            if (winnerOpt.isDefined) {
                this.winner = winnerOpt.get match {
                    case GamePlayerX => GameWinnerX
                    case GamePlayerO => GameWinnerO
                }

                return this.winner
            }
        }

        this.winner = GameWinnerNone
        this.winner
    }

    def checkIfWon(posH: Int, posW: Int): Option[GamePlayer] = {
        val move: GamePlayer = gameMap((posH, posW))

        for (i <- 0 until Game.dirGroupsCnt) {
            val (leftDir, rightDir) = Game.dirs(i)

            var inARowLeft: Int = 1
            var inARowRight: Int = 1
            while (gameMap.contains((posH + Game.gameDirH(leftDir) * inARowLeft, posW + Game.gameDirW(leftDir) * inARowLeft)) &&
                gameMap((posH + Game.gameDirH(leftDir) * inARowLeft, posW + Game.gameDirW(leftDir) * inARowLeft)) == move) {

                inARowLeft += 1
            }

            while (gameMap.contains((posH + Game.gameDirH(rightDir) * inARowRight, posW + Game.gameDirW(rightDir) * inARowRight)) &&
                gameMap((posH + Game.gameDirH(rightDir) * inARowRight, posW + Game.gameDirW(rightDir) * inARowRight)) == move) {

                inARowRight += 1
            }

            if (inARowLeft + inARowRight - 1 >= 5) {
                // We have a winner!
                return Option(gameMap((posH, posW)))
            }
        }

        None
    }
}

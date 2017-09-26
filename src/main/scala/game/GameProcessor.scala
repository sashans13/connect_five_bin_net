package game

import common.Constants
import node.Node
import tokens.GameToken

import scala.collection.mutable
import scala.util.Random

class GameProcessor(gameToken: GameToken, node: Node) extends Serializable {
    // First startingMoves bits represent the start of a game
    var gamesToPlay: mutable.Stack[Int] = new mutable.Stack[Int]()
    private val gamesLock: Object = new Object

    var gamesWonByX: Int = 0
    var gamesWonByO: Int = 0
    var gamesWonByNone: Int = 0
    private val winnerLock: Object = new Object

    def startWorking(): Unit = {
        while (this.gamesToPlay.nonEmpty) {
            val gamesBucket: mutable.Stack[Int] = new mutable.Stack[Int]()

            this.gamesLock.synchronized {
                var i = 0
                while (this.gamesToPlay.nonEmpty && i < Constants.GAME_BUCKET_SIZE) {
                    gamesBucket.push(this.gamesToPlay.pop())
                    i += 1
                }
            }

            // Account for winners
            val winners: Seq[GameWinner] = gamesBucket.map {
                start =>
                    new Game(start, gameToken.numStartingMoves, gameToken.dimH, gameToken.dimW).getWinner
            }

            this.winnerLock.synchronized {
                winners.foreach {
                    case GameWinnerX => gamesWonByX += 1
                    case GameWinnerO => gamesWonByO += 1
                    case GameWinnerNone => gamesWonByNone += 1
                }

            }

            // Update neighbours with redundancy
            this.node.storeSnapshotWithNeighbours()
        }
    }

    def addGamesFromRangesAndCount(startRange: Int, endRange: Int, numGames: Int): Unit = {
        if (endRange - startRange < numGames) {
            throw new Exception("endRange - startRange < numGames, whader ya thinkin??!?!?")
        }

        val set = mutable.Set[Int]()
        val rand = new Random()

        while (set.size < numGames) {
            // Generate number in [startRange, endRange) and add to set
            set.add(rand.nextInt(endRange - startRange) + startRange)
        }

        // Add all games to stack
        this.gamesLock.synchronized {
            set.map(gamesToPlay.push(_))
        }

        this.startWorking()
    }

    def mergeWith(other: GameProcessor): GameProcessor = {
        // Games
        other.gamesToPlay.foreach {
            gameToPlay => this.gamesToPlay.push(gameToPlay)
        }

        // Winners
        this.gamesWonByX += other.gamesWonByX
        this.gamesWonByO += other.gamesWonByO
        this.gamesWonByNone += other.gamesWonByNone

        this
    }
}

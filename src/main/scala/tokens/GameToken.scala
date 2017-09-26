package tokens

import java.util.Calendar

import common.{ConnectionInfo, Constants, Util}

import scala.collection.mutable

object GameToken {
    /**
      * Returns (startRangeInclusive, endRangeNONinclusive, numGamesToSimulateForRange)
      */
    def calculateRangesAndGames(numGames: Int, numNodes: Int): Seq[(Int, Int, Int)] = {
        val pow2 = Util.getSmallestPow2GreaterThanOrEqN(numGames)

        // Every node gets
        val rangeSize = pow2 / numNodes

        Seq.range(0, numNodes)
            .map {
                case rangeId if rangeId < numNodes - 1 =>
                    (rangeSize * rangeId, rangeSize * (rangeId + 1), numGames / numNodes)
                case rangeId if rangeId == numNodes - 1=>
                    (rangeSize * rangeId, pow2, numGames / numNodes + numGames % numNodes)

            }
    }
}

class GameToken(numGames: Int, numNodes: Int, val dimH: Int, val dimW: Int) extends Serializable {
    // Set local timestamp
    val createdAt: Long = Calendar.getInstance().getTimeInMillis
    val id: Long = createdAt // Yeah, id is the same thing as createdAt

    // Used in combo with id to check if properly walking the path (taking care of node failures)
    var traversalsCnt: Int = 0

    // Number of starting moves, based on position of left-most bit in pow2
    val numStartingMoves: Int = Util.log2floor(Util.getSmallestPow2GreaterThanOrEqN(numGames) + 1)

    // Games finished (this is updated by status token)
    var gamesFinished: Int = 0

    // Generate game buckets
    var gameBuckets: mutable.Stack[(Int, Int, Int)] = {
        val gamesInfo: mutable.Stack[(Int, Int, Int)] = mutable.Stack[(Int, Int, Int)]()

        // Get game ranges but take into account that numNodes might be < actual number of nodes
        val gameRanges: Seq[(Int, Int, Int)] = GameToken.calculateRangesAndGames(numGames, numNodes * Constants.GAME_CNT_LEEWAY_FACTOR)

        // Populate gamesInfo from gameRanges
        gameRanges.map(x => gamesInfo.push(x))

        gamesInfo
    }

    // Previous 2 nodes visited
    var prevVisited: ConnectionInfo = _
    var prevVisited2: ConnectionInfo = _

    def shiftVisited(conn: ConnectionInfo) = {
        prevVisited2 = prevVisited
        prevVisited = conn
    }

    def notifyPrevVisited2(): Boolean = {
        if (prevVisited2 == null) {
            return true
        }


        // Open socket to prev2
        val (socket, in, out) = prevVisited2.connect

        out.writeObject("game_token_notify")
        out.writeObject(this.createdAt)
        out.flush()

        val resp: String = in.readObject().asInstanceOf[String]

        socket.close()

        resp == "game_token_notify_ack"
    }

    // Such copy, much oop
    def notifyPrevVisited(): Boolean = {
        if (prevVisited == null) {
            return true
        }


        // Open socket to prev2
        val (socket, in, out) = prevVisited.connect

        out.writeObject("game_token_notify")
        out.writeObject(this.createdAt)
        out.flush()

        val resp: String = in.readObject().asInstanceOf[String]

        socket.close()

        resp == "game_token_notify_ack"
    }

    def sendToNext(next: ConnectionInfo): Unit = {
        // Open socket to next
        val (socket, in, out) = next.connect

        out.writeObject("incoming_game_token")
        out.writeObject(this)
        out.flush()

        socket.close()
    }
}

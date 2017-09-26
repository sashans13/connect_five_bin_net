package tokens

import common.ConnectionInfo

class StatusToken(val gameTokenId: Long, numGames: Int) extends Serializable {
    var gamesComplete: Int = 0

    def percentComplete: Double = gamesComplete.toDouble / numGames.toDouble

    def sendToNext(next: ConnectionInfo): Unit = {
        // Open socket to next
        val (socket, in, out) = next.connect

        out.writeObject("incoming_status_token")
        out.writeObject(this)
        out.flush()

        socket.close()
    }
}

package node

import common.{ConnectionInfo, Constants}

class NodeHealthChecker(node: Node) extends Runnable {
//    private val notifyLock = new Object

    override def run(): Unit = {
        // Sleep
        try {
            Thread.sleep(Constants.HEALTHCHECK_INTERVAL_MS)
        }
        catch {
            case ie: InterruptedException => ???
            case ex: Exception => throw ex
        }

        // Do actual work
        try {
            // Ping right neighbour to see if it's alive
            val (socketNeighbour, in, out) = node.getNeighbours(TraversalRightNeighbour).connect
            out.writeObject("ping")
            out.flush()
            val resp: String = in.readObject().asInstanceOf[String]
            if (resp != "pong") {
                throw new Exception("ping didn't get its pong :/")
            }
            socketNeighbour.close()
        }
        catch {
            case ex: Exception =>
                // TODO: get shit done
                var lastNode: ConnectionInfo = null

                // Get last node and try reconstruct until success
                do {
                    lastNode = node.getLastNode
                } while (!this.tryReconstruct(lastNode))
        }
    }

    def tryReconstruct(lastNode: ConnectionInfo): Boolean = {
        val (socket, in, out) = lastNode.connect

        out.writeObject("handle_failure")
        out.flush()
        val reserveResp = in.readObject().asInstanceOf[String]

        if (reserveResp == "reserve_success") {
            out.writeObject(this.node.asConnectionInfo)
            out.flush()
            val doneResp = in.readObject().asInstanceOf[String]

            if (doneResp == "reconstruction_done") {
                socket.close()
                return true
            } // else is reconstruction_fail
        } // else is reserve_fail

        socket.close()
        false
    }
}

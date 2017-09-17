package node

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.net.Socket

import common.ConnectionInfo

import scala.util.control.Breaks._

class NodeClientHandler(clientSocket: Socket, node: Node) extends Runnable {

    override def run(): Unit = {
        val in = new ObjectInputStream(clientSocket.getInputStream)
        val out = new ObjectOutputStream(clientSocket.getOutputStream)

        while (!clientSocket.isClosed) {
            // Communication protocol
            val message: String = in.readObject().asInstanceOf[String]

            breakable {
                // break inside breakable evaluates to continue if there's nothing in the loop after breakable{}
                // =============== ping - pong ===============
                if (message == "ping") {
                    out.writeObject("pong")
                    out.flush()

                    clientSocket.close()
                    break // Is actually continue when in breakable{}
                }

                if (message == "neighbours") {
                    out.writeObject(node.getNeighbours)
                    out.flush()

                    clientSocket.close()
                    break
                }

                if (message == "join") {
                    node.neighboursLock.synchronized {
                        val joinConnInfo = in.readObject().asInstanceOf[ConnectionInfo]
                        val traversalDirOpt: Option[TraversalDirection] = node.nodeId.getTraversalTo(joinConnInfo.nodeId)

                        if (traversalDirOpt.isDefined) {
                            out.writeObject("join_accepted")
                            out.flush()

                            val message = in.readObject().asInstanceOf[String]
                            if (message == "join_ack") {
                                this.node.addNeighbour(traversalDirOpt.get, joinConnInfo)
                            }
                        } else {
                            out.writeObject("join_denied")
                            out.flush()
                        }

                        clientSocket.close()
                        break
                    }
                }

                throw new Exception("NodeClientHandler: Unknown message: " + message)
            }
        }
    }
}

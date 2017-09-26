package node

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.net.Socket

import common.ConnectionInfo
import tokens.{GameToken, StatusToken}

import scala.util.control.Breaks._

class NodeClientHandler(clientSocket: Socket, node: Node) extends Runnable {

    override def run(): Unit = {
        val out = new ObjectOutputStream(clientSocket.getOutputStream)
        val in = new ObjectInputStream(clientSocket.getInputStream)

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

                if (message == "game_token_notify") {
                    val gameTokenId = in.readObject().asInstanceOf[Long]

                    // Set retries to 0 do it doesn't send again
                    this.node.tokenRetriesLeft = 0

                    out.writeObject("game_token_notify_ack")
                    out.flush()

                    clientSocket.close()
                    break
                }

                if (message == "incoming_game_token") {
                    val gameToken = in.readObject().asInstanceOf[GameToken]

                    node.takeCareOfGameToken(gameToken)

                    clientSocket.close()
                    break
                }

                if (message == "incoming_status_token") {
                    val statusToken = in.readObject().asInstanceOf[StatusToken]

                    node.takeCareOfStatusToken(statusToken)

                    clientSocket.close()
                    break
                }

                if (message == "child_report") {
                    val child: ConnectionInfo = in.readObject().asInstanceOf[ConnectionInfo]

                    if (child.nodeId.isLeftChildOf(this.node.nodeId)) {
                        this.node.addNeighbour(TraversalLeftChild, child)
                    } else if (child.nodeId.isRightChildOf(this.node.nodeId)) {
                        this.node.addNeighbour(TraversalRightChild, child)
                    } else {
                        throw new Exception("Child is not actually a child!")
                    }

                    clientSocket.close()
                    break
                }

                if (message == "left_neighbour") {
                    val leftNeighbour: ConnectionInfo = in.readObject().asInstanceOf[ConnectionInfo]
                    node.addNeighbour(TraversalLeftNeighbour, leftNeighbour)

                    out.writeObject("left_neighbour_ack")
                    out.flush()

                    clientSocket.close()
                    break
                }

                if (message == "want_store_snapshot") {
                    val nodeConnInfo = in.readObject().asInstanceOf[ConnectionInfo]
                    val nodeSnapshot = in.readObject().asInstanceOf[NodeSnapshot]

                    node.addSnapshot(nodeConnInfo, nodeSnapshot)

                    clientSocket.close()
                    break
                }

                if (message == "handle_failure") {
                    val reservationSuccessful = node.tryReserve
                    if (reservationSuccessful) {
                        out.writeObject("reservation_success")
                        val leftNeighbourOfFailedNode: ConnectionInfo = in.readObject().asInstanceOf[ConnectionInfo]

                        this.node.reposition(leftNeighbourOfFailedNode)
                    } else {
                        out.writeObject("reservation_fail")
                    }

                    clientSocket.close()
                    break
                }

                if (message == "want_snapshot") {
                    val traversalDir: TraversalDirection = in.readObject().asInstanceOf[TraversalDirection]

                    out.writeObject(this.node.getSnapshot(traversalDir))
                    out.flush()

                    clientSocket.close()
                    break
                }

                if (message == "notify_new_neighbour") {
                    val newNeighbour = in.readObject().asInstanceOf[ConnectionInfo]
                    val traversalToNeighbour: TraversalDirection = this.node.nodeId.getTraversalTo(newNeighbour.nodeId).get

                    this.node.addNeighbour(traversalToNeighbour, newNeighbour)

                    clientSocket.close()
                    break
                }

                if (message == "want_join") {
                    node.neighboursLock.synchronized {
                        // Check if we are the last node
                        if (node.getNeighbours(TraversalRightNeighbour).nodeId.id != 1) {
                            // Faaaailed, we are not last node
                            node.contextPrintln("NodeClientHandler: run: Failed to provide join id.")
                            out.writeObject("join_declined")
                            out.flush()
                            clientSocket.close()
                            break
                        }

                        // Accept and give node the next id
                        out.writeObject("join_accepted")
                        out.writeObject(NodeId(node.nodeId.id + 1))
                        out.writeObject(Option(node.getNeighbours(TraversalRightNeighbour)))
                        out.flush()

                        val joinConnInfoOpt: Option[ConnectionInfo] = in.readObject().asInstanceOf[Option[ConnectionInfo]]
                        if (joinConnInfoOpt.isEmpty) {
                            node.contextPrintln("NodeClientHandler: run: Joining node - NACK")
                        } else {
                            println("wants join: " + joinConnInfoOpt)
                            node.addNeighbour(TraversalRightNeighbour, joinConnInfoOpt.get)
                            node.contextPrintln("NodeClientHandler: run: Joining node - ACK")
                            out.writeObject("join_ack")
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

package bootstrap

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.net.Socket

import common.ConnectionInfo
import node.NodeId

import scala.util.control.Breaks._

// Note: BootstrapHandler should NOT start a new thread in itself
class BootstrapHandler(clientSocket: Socket) extends Runnable {
    val out: ObjectOutputStream = new ObjectOutputStream(clientSocket.getOutputStream)
    val in: ObjectInputStream = new ObjectInputStream(clientSocket.getInputStream)

    override def run(): Unit = {
        while (!clientSocket.isClosed) {
            // Receive a new message
            val initialMessage: String = in.readObject().asInstanceOf[String]

            breakable {
                initialMessage match {
                    case "connect" =>
                        // Get other node's info
                        Bootstrap.connectLock.synchronized {
                            println("BootstrapHandler: run: entered connect block")

                            if (Bootstrap.nodes.isEmpty) {
                                // This is initial node, send None
                                out.writeObject(None)
                                out.flush()

                            } else {
                                // This is NOT an initial node

                                // Return:
                                // highest NodeId,                  if NodeId != rightTreeEdge
                                // first NodeId in last row,        otherwise
                                val (lastNodeId, lastConnInfo): (NodeId, ConnectionInfo) = Bootstrap.nodes.toList.maxBy(_._1.id)
//                                if (lastNodeId.id != 1 && lastNodeId.isRightEdgeInGraph) {
//                                    val firstInLastRow = Bootstrap.nodes(NodeId(lastNodeId.id / 2 + 1))
//                                    out.writeObject(Option(firstInLastRow))
//                                } else {
                                    out.writeObject(Option(lastConnInfo))
//                                }
                                out.flush()
                            }

                            // This is also considered an ack
                            val nodeConnInfo: ConnectionInfo = in.readObject().asInstanceOf[ConnectionInfo]
                            Bootstrap.addNode(nodeConnInfo)

                            println("BootstrapHandler: run: left connect block")
                        }
                }

                clientSocket.close()
                break
            }
        }
    }
}
package bootstrap

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.net.Socket

import common.ConnectionInfo

import scala.util.Random
import scala.util.control.Breaks._

// Note: BootstrapHandler should NOT start a new thread in itself
class BootstrapHandler(clientSocket: Socket) extends Runnable {
    val out: ObjectOutputStream = new ObjectOutputStream(clientSocket.getOutputStream)
    val in: ObjectInputStream = new ObjectInputStream(clientSocket.getInputStream)

    override def run(): Unit = {
        while(!clientSocket.isClosed) {
            // Receive a new message
            val initialMessage: String = in.readObject().asInstanceOf[String]

            breakable {
                initialMessage match {
                    case "connect" =>
                        // Get other node's info
                        Bootstrap.connectLock.synchronized {
                            println("BootstrapHandler: run: entered connect block")

                            val clientNodeConnInfo: ConnectionInfo = in.readObject().asInstanceOf[ConnectionInfo]

                            if (Bootstrap.nodes.isEmpty) {
                                // This is initial node, send None
                                val noneOpt: Option[ConnectionInfo] = None
                                out.writeObject(noneOpt)
                                out.flush()

                            } else {
                                // This is NOT an initial node

                                // Pick a random connection
                                val valuesArray: Seq[ConnectionInfo] = Bootstrap.nodes.values.toList
                                val r: Random = new Random
                                val connToReturn: ConnectionInfo = valuesArray(r.nextInt(valuesArray.size))

                                out.writeObject(Option(connToReturn))
                                out.flush()
                            }

                            // Add node after it has connected (so we know nodeId too)
                            Bootstrap.addNode(clientNodeConnInfo)

                            println("BootstrapHandler: run: left connect block")
                        }
                }

                clientSocket.close()
                break
            }
        }
    }
}

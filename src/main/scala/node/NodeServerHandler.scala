package node

import java.net.Socket

class NodeServerHandler(node: Node) extends Runnable {
    override def run(): Unit = {
        while (true) {
            val clientSocket: Socket = node.serverSocket.accept()
            // Accept incoming connection in a new Thread
            new Thread(new NodeClientHandler(clientSocket, node)).start()
        }
    }
}

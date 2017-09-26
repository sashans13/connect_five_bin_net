package bootstrap

import java.net.ServerSocket
import java.util.concurrent.ConcurrentHashMap

import node.NodeId
import common._

import scala.collection.JavaConverters._
import scala.collection.concurrent.{Map => ConcurrentMap}

object Bootstrap {
    val nodes: ConcurrentMap[NodeId, ConnectionInfo] = new ConcurrentHashMap[node.NodeId, ConnectionInfo]().asScala
    val connectLock: Object = new Object

    def startServer(runMode: RunMode): Unit = {
        // Open a socket, if it fails, bootstrap is probably already running
        val serverSocket = new ServerSocket(Constants.BOOTSTRAP_SERVER_PORT)

        runMode match {
            // This process can run only one thread which is blocking for everyone
            case ProcessMode =>
                new Bootstrap(serverSocket).run()

            // In thread mode, we run all nodes as threads, so this can't be blocking
            case ThreadMode =>
                new Thread(new Bootstrap(serverSocket)).start()
        }
    }

    def addNode(nodeConnInfo: ConnectionInfo): Unit = {
        // TODO: lock?
        this.nodes(nodeConnInfo.nodeId) = nodeConnInfo
    }
}

class Bootstrap(serverSocket: ServerSocket) extends Runnable {
    def run(): Unit = {
        while (true) {
            // This blocks until a connection comes in
            val socket = serverSocket.accept()
            new Thread(new BootstrapHandler(socket)).start()
        }
    }
}
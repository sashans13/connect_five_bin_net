package common

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.net.Socket

import node.NodeId

object ConnectionInfo {
    def empty = new ConnectionInfo("0.0.0.0", 0, NodeId.default)
}

class ConnectionInfo(val ipAddress: String, val port: Int, val nodeId: NodeId) extends Serializable {
    def connect: (Socket, ObjectInputStream, ObjectOutputStream) = {
        val socket: Socket = new Socket(this.ipAddress, this.port)

        (socket, new ObjectInputStream(socket.getInputStream), new ObjectOutputStream(socket.getOutputStream))
    }

    def isEmpty: Boolean = this == ConnectionInfo.empty

    override def toString = nodeId + " @ " + ipAddress + ":" + port
}

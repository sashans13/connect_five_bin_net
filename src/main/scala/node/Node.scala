package node

import java.net.{InetAddress, ServerSocket}
import java.util.concurrent.ConcurrentHashMap

import common.{ConnectionInfo, Constants, Util}
import node.Node.{NeighbourMapJava, NeighbourMapRef}

import scala.collection.JavaConverters._
import scala.collection.concurrent.{Map => ConcurrentMap}
import scala.collection.mutable
import scala.util.control.Breaks._

object Node {
    type NeighbourMapJava = ConcurrentHashMap[TraversalDirection, ConnectionInfo]
    type NeighbourMapRef = ConcurrentMap[TraversalDirection, ConnectionInfo]

    def contextPrintln(node: Node, string: String): Unit = {
        val nodeInfo = "[Node " + node.nodeId + " @ " + node.ipAddress + ":" + node.port + "]\n"
        println("\n" + nodeInfo + ">> " + string + "\n")
    }

    def hasChildren(neighbourMapRef: NeighbourMapRef): Boolean =
        neighbourMapRef.contains(TraversalLeftChild) ||
            neighbourMapRef.contains(TraversalRightChild)

    def getNeighboursFromNode(nodeConnInfo: ConnectionInfo): NeighbourMapRef = {
        val (_, in, out) = nodeConnInfo.connect

        out.writeObject("neighbours")
        // TODO: This might be a wrong cast
        in.readObject().asInstanceOf[NeighbourMapJava].asScala
    }
}

class Node extends Runnable {
    val serverSocket: ServerSocket = Util.tryBindSocket(Constants.PORT_RANGE_START, Constants.PORT_RANGE_WIDTH)

    var nodeId: NodeId = NodeId.default // Default node id
    var state: NodeState = Initial

    // Network related
    val ipAddress: String = InetAddress.getLocalHost.getHostAddress
    val port: Int = serverSocket.getLocalPort
    val asConnectionInfo = new ConnectionInfo(this.ipAddress, this.port, this.nodeId)

    private val neighbours: NeighbourMapRef = new NeighbourMapJava().asScala
    val neighboursLock: Object = new Object

    override def run(): Unit = {
        // TODO: Implement server handler

        val (bootstrapSocket, in, out) = new ConnectionInfo(Constants.BOOTSTRAP_IP_ADDRESS, Constants.BOOTSTRAP_SERVER_PORT, NodeId(0)).connect

        var joinAttempts = 0
        while (!bootstrapSocket.isClosed && joinAttempts < Constants.MAX_JOIN_ATTEMPTS) {
            breakable {
                joinAttempts += 1

                out.writeObject("connect")
                out.writeObject(this.asConnectionInfo)
                out.flush()

                // Get a random node from bootstrap
                val hookConnectionInfoOpt: Option[ConnectionInfo] = in.readObject().asInstanceOf[Option[ConnectionInfo]]

                if (hookConnectionInfoOpt.isEmpty) {
                    // Initial node if there is no connection

                    this.nodeId = NodeId(1)

                    contextPrintln("Node: run: Initial node joined chord.")
                } else {
                    // Not initial node
                    if (joinGraph(hookConnectionInfoOpt.get)) {
                        // TODO: Update node state and connections
                        contextPrintln("Node: run: Non-initial node joined chord.")

                        // Start server handler
                        new Thread(new NodeServerHandler(this)).start()
                    } else {
                        // Failed to join
                        contextPrintln("Node: run: Failed to join, probably retrying...")
                        break
                    }
                }

                // We joined the graph
                this.state = Connected

                if (!bootstrapSocket.isClosed) {
                    bootstrapSocket.close()
                }
            }
        }
    }


    def joinGraph(currConnectionInfo: ConnectionInfo): Boolean = {
        // Traverse the shit out of it

        // === Try to join

        // Find joinable node
        // Let "joinable node" be a node at which a new node can join
        // Then, joinable node is:
        // case last row not full: node with highest id
        // case last row is full: left-most node in first row
        val joinableNodeAndDirOpt: Option[(ConnectionInfo, TraversalDirection)] = findJoinableNode(currConnectionInfo)
        if (joinableNodeAndDirOpt.isEmpty) {
            // Traversal failed, return false so it may try agin
            contextPrintln("Node: joinGraph: Traversal failed for node.")
            return false
        }

        // Try join, return false if fail
        tryJoin(joinableNodeAndDirOpt.get._1, joinableNodeAndDirOpt.get._2)
    }

    def tryJoin(nodeToJoin: ConnectionInfo, traversalDirection: TraversalDirection): Boolean = {
        // Choose a direction
        // 1 - try right
        // 2 - try left child

        val (socket, in, out) = nodeToJoin.connect

        out.writeObject("join")

        // Set correct node id
        this.nodeId = nodeToJoin.nodeId.getRightNeighbourId
        out.writeObject(this.asConnectionInfo)

        val resp = in.readObject().asInstanceOf[String]

        if (resp != "join_accepted") {
            // Revert to default node id
            this.nodeId = NodeId.default

            socket.close()
            return false
        }

        // resp == "join_accepted"
        out.writeObject("join_ack")
        out.flush()

        val traversalDirToNodeToJoinOpt = this.nodeId.getTraversalTo(nodeToJoin.nodeId)
        this.addNeighbour(traversalDirToNodeToJoinOpt.get, nodeToJoin)

        socket.close()
        true
    }
//
//    /**
//      * Returns NODE to join, and traversal direction from NODE
//      * @param startingPoint
//      * @return
//      */
//    def findJoinableNode(startingPoint: ConnectionInfo): Option[(ConnectionInfo, TraversalDirection)] = {
//        var currConnInfo: ConnectionInfo = startingPoint
//        var prevId: NodeId = NodeId(0)
//        var currNeighbours: NeighbourMapRef = Node.getNeighboursFromNode(startingPoint)
//
//        var depthDebounce: Int = 0 // Node 1 is on depth 1, so use 0 here
//
//
//        // Check if only 1 node in graph
//        if (currConnInfo.nodeId.id == 1 && !Node.hasChildren(currNeighbours)) {
//            return Option(currConnInfo, TraversalLeftChild)
//        }
//
//        // Check where to go for initial move
//        if (currConnInfo.nodeId.isLeftEdgeInGraph) {
//
//            if (currNeighbours.contains(TraversalLeftChild)) {
//                currConnInfo = currNeighbours(TraversalLeftChild)
//            } else if (currNeighbours.contains(TraversalRightNeighbour)) {
//                currConnInfo = currNeighbours(TraversalRightNeighbour)
//            } else {
//                // Nothing left to do here
//                return Option(currConnInfo, TraversalRightNeighbour)
//            }
//        } else {
//            // !currConnInfo.nodeId.isLeftEdgeInGraph)
//            // Should always have left neighbor
//            prevId = currConnInfo.nodeId
//            currConnInfo = currNeighbours(TraversalLeftNeighbour)
//        }
//        // Current id is now the prevId
//        prevId = currConnInfo.nodeId
//
//        while (true) {
//            // Go leftmost
//            while (!currConnInfo.nodeId.isLeftEdgeInGraph) {
//                currNeighbours = Node.getNeighboursFromNode(currConnInfo)
//
//                prevId = currConnInfo.nodeId
//                currConnInfo = currNeighbours(TraversalLeftNeighbour)
//            }
//
//            // Go down by left children
//            currNeighbours = Node.getNeighboursFromNode(currConnInfo)
//            while (currNeighbours.contains(TraversalLeftChild)) {
//                prevId = currConnInfo.nodeId
//                currConnInfo = currNeighbours(TraversalLeftChild)
//                currNeighbours = Node.getNeighboursFromNode(currConnInfo)
//            }
//
//            if (currConnInfo.nodeId.depth == depthDebounce) {
//                // We already visited right, return curr
//                return Option(currConnInfo, TraversalLeftChild)
//            }
//
//            // Go right
//            while (currNeighbours.contains(TraversalRightNeighbour)) {
//                prevId = currConnInfo.nodeId
//                currConnInfo = currNeighbours(TraversalRightNeighbour)
//                currNeighbours = Node.getNeighboursFromNode(currConnInfo)
//            }
//
//            // If not on right edge, currConnInfo is the joinable node
//            if (!currConnInfo.nodeId.isRightEdgeInGraph) {
//                return Option(currConnInfo, TraversalRightNeighbour)
//            } else {
//                // We on the right edge, gotta go back
//                depthDebounce = currConnInfo.nodeId.depth
//            }
//        }
//
//        throw new Exception("Should never get here")
//    }



    def getNeighbours: mutable.Map[TraversalDirection, ConnectionInfo] = this.neighbours.clone()

//    def traverse(traversalGoal: TraversalGoal, prevId: NodeId): (TraversalDirection, Option[])= {
//        traversalGoal match {
//            case FindJoinableNode =>
//                // We didn't move
//                if(prevId.id == this.nodeId.id) {
//                    return
//                }
//                // If final node
//                if (
//                    !this.neighbours.contains(TraversalLeftChild) &&
//                    !this.neighbours.contains(TraversalRightChild) &&
//                    !this.neighbours.contains(TraversalRightNeighbour)
//                )
////                if (prevId.id == this.nodeId.id) {
////                    return
////                }
//        }
//
//    }
//
//    def traverse(traversalGoal: TraversalGoal, prevId: NodeId)

    def addNeighbour(traversalDir: TraversalDirection, neighbour: ConnectionInfo): Unit = {
        this.neighbours(traversalDir) = neighbour
    }

    def contextPrintln(string: String): Unit = Node.contextPrintln(this, string)
}

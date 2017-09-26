package node

import java.net.{InetAddress, ServerSocket}
import java.util.concurrent.ConcurrentHashMap

import common.{ConnectionInfo, Constants, Util}
import game.GameProcessor
import node.Node._
import tokens.{GameToken, StatusToken}

import scala.collection.JavaConverters._
import scala.collection.concurrent.{Map => ConcurrentMap}
import scala.util.control.Breaks._

object Node {
    type ConcurrentMapJava = ConcurrentHashMap[TraversalDirection, ConnectionInfo]
    type ConcurrentMapRef = ConcurrentMap[TraversalDirection, ConnectionInfo]

    type TokenLastVisitedMapJava = ConcurrentHashMap[Long, Long]
    type TokenLastVisitedMapRef = ConcurrentMap[Long, Long]

    type SnapshotsMapJava = ConcurrentHashMap[TraversalDirection, NodeSnapshot]
    type SnapshotsMapRef = ConcurrentMap[TraversalDirection, NodeSnapshot]

    type StatusTokenMapJava = ConcurrentHashMap[Long, Int]
    type StatusTokenMapRef = ConcurrentMap[Long, Int]

    def contextPrintln(node: Node, string: String): Unit = {
        val nodeInfo = "[Node " + node.nodeId + " @ " + node.ipAddress + ":" + node.port + "]\n"
        println("\n" + nodeInfo + ">> " + string + "\n")
    }

    def hasChildren(neighbourMapRef: ConcurrentMapRef): Boolean =
        neighbourMapRef.contains(TraversalLeftChild) ||
            neighbourMapRef.contains(TraversalRightChild)

    def getNeighboursFromNode(nodeConnInfo: ConnectionInfo): Map[TraversalDirection, ConnectionInfo] = {
        val (_, in, out) = nodeConnInfo.connect

        out.writeObject("neighbours")
        out.flush()

        // TODO: This might be a wrong cast
        in.readObject().asInstanceOf[Map[TraversalDirection, ConnectionInfo]]
    }

    def createSnapshot(node: Node): NodeSnapshot = {
        val nodeSnapshot: NodeSnapshot = new NodeSnapshot

        nodeSnapshot.nodeId = node.nodeId
        nodeSnapshot.neighbours = node.neighbours
        nodeSnapshot.gameToken = node.currentGameToken
//        nodeSnapshot.gameProcessor = node.currentGameProcessor

        nodeSnapshot
    }
}

class Node(var deleteThis: String) extends Runnable {
    val serverSocket: ServerSocket = Util.tryBindSocket(Constants.PORT_RANGE_START, Constants.PORT_RANGE_WIDTH)

    var nodeId: NodeId = NodeId.default // Default node id
    private var state: NodeState = Initial
    val stateLock: Object = new Object

    // Network related
    val ipAddress: String = InetAddress.getLocalHost.getHostAddress
    val port: Int = serverSocket.getLocalPort

    private var neighbours: ConcurrentMapRef = new ConcurrentMapJava().asScala
    val neighboursLock: Object = new Object

    // TODO: Update snapshot on every change
    private val snapshots: SnapshotsMapRef = new SnapshotsMapJava().asScala
    private val snapshotsLock: Object = new Object

    // Tokens
//    private val gameTokens: ConcurrentMapRef = new ConcurrentMapJava().asScala
    private val tokenLastVisited: TokenLastVisitedMapRef = new TokenLastVisitedMapJava().asScala
    private var currentGameToken: GameToken = _
    private val statusTokenLastVisited: StatusTokenMapRef = new StatusTokenMapJava().asScala

    // Flag that says how many to send token until it succeeds
    var tokenRetriesLeft: Int = _

    private var currentGameProcessor: GameProcessor = _

    def asConnectionInfo = new ConnectionInfo(this.ipAddress, this.port, this.nodeId)

    override def run(): Unit = {
        // TODO: Implement server handler

        val (bootstrapSocket, in, out) = new ConnectionInfo(Constants.BOOTSTRAP_IP_ADDRESS, Constants.BOOTSTRAP_SERVER_PORT, NodeId.default).connect

        var joinAttempts = 0
        while (!bootstrapSocket.isClosed && joinAttempts < Constants.MAX_JOIN_ATTEMPTS) {
            breakable {
                joinAttempts += 1

                out.writeObject("connect")
                out.flush()

                // Get a random node from bootstrap
                val hookConnectionInfoOpt: Option[ConnectionInfo] = in.readObject().asInstanceOf[Option[ConnectionInfo]]

                if (joinGraph(hookConnectionInfoOpt)) {
                    contextPrintln("Node: run: Finished joining the graph.")
                } else {
                    // Failed to join
                    contextPrintln("Node: run: Failed to join, probably retrying...")
                    break
                }

                out.writeObject(this.asConnectionInfo)
                out.flush()

                if (!bootstrapSocket.isClosed) {
                    bootstrapSocket.close()
                }
            }
        }

        if (this.deleteThis == "DELETE THIS") {
            Thread.sleep(2000)
            this.contextPrintln("STARTING TOKEN")
            this.tryStartGame(10, 10, 500)
        }
    }


    def joinGraph(hookConnInfoOpt: Option[ConnectionInfo]): Boolean = {
        // Assume given the last node
        hookConnInfoOpt match {
            case Some(hookConnInfo) =>
                // This node is NOT initial
                this.contextPrintln("Node: joinGraph: Non-initial node" +
                "\nHook: " + hookConnInfo)

                // Ask hook to join
                val (hookSocket, in, out) = hookConnInfo.connect

                out.writeObject("want_join")
                out.flush()

                val resp: String = in.readObject().asInstanceOf[String]

                resp match {
                    case "join_accepted" =>
                        val mySuggestedId = in.readObject().asInstanceOf[NodeId]

                        // This should always be leftNeighbour
                        val traversalToHookOpt: Option[TraversalDirection] = mySuggestedId.getTraversalTo(hookConnInfo.nodeId)
                        val hooksRightNeighbourOpt = in.readObject().asInstanceOf[Option[ConnectionInfo]]

                        if (traversalToHookOpt.isEmpty) {
                            // Hook didn't provide a neighbouring id
                            this.contextPrintln("Node: joinGraph: hook didn't provide a neighbouring id")

                            // Instead of sending our connInfo, send None, so hook knows join failed
                            out.writeObject(None)
                            out.flush()

                            hookSocket.close()
                            return false
                        }

                        if (hooksRightNeighbourOpt.isEmpty || hooksRightNeighbourOpt.get.nodeId.id != 1) {
                            this.contextPrintln("Node: joinGraph: hook's right neighbour empty or not 1")

                            out.writeObject(None)
                            out.flush()

                            hookSocket.close()
                            return false
                        }

                        this.nodeId = mySuggestedId
                        this.addNeighbour(traversalToHookOpt.get, hookConnInfo)
                        this.addNeighbour(TraversalRightNeighbour, hooksRightNeighbourOpt.get)

                        // Notify right neighbour we are his left
                        val (hooksRNSocket, rnIn, rnOut) = hooksRightNeighbourOpt.get.connect
                        rnOut.writeObject("left_neighbour")
                        rnOut.writeObject(this.asConnectionInfo)
                        rnOut.flush()
                        val rnResp = rnIn.readObject().asInstanceOf[String]
                        if (rnResp != "left_neighbour_ack") {
                            return false
                        }
                        hooksRNSocket.close()

                        // Ack to hook
                        out.writeObject(Option(this.asConnectionInfo))
                        out.flush()

                        // Hook ack to us
                        in.readObject()
                        hookSocket.close()

                        // TODO: Connect to parent too
                        this.addNeighbour(TraversalParent, this.getParent(hookConnInfo))
                        // Let parent know
                        val (parentSocket, inParent, outParent) = this.neighbours(TraversalParent).connect
                        outParent.writeObject("child_report")
                        outParent.writeObject(this.asConnectionInfo)
                        outParent.flush()
                        parentSocket.close()
                    case _ =>
                        // Join not accepted
                        this.contextPrintln("Node: joinGraph: Join not accepted by: " + hookConnInfo.toString)

                        hookSocket.close()
                        return false
                }

            case None =>
                // This is initial node
                this.contextPrintln("Node: joinGraph: Initial node")
                this.nodeId = NodeId(1)

                // Add self to the left and right to maintain circularity
                this.addNeighbour(TraversalRightNeighbour, this.asConnectionInfo)
                this.addNeighbour(TraversalLeftNeighbour, this.asConnectionInfo)
        }

        // We joined the graph and have no work
        this.state = IdleNoWork

        // Start the server handler for this node
        new Thread(new NodeServerHandler(this)).start()

        contextPrintln("Node: joinGraph: we good!")
        true
    }

    // Such hack, much cheat...
    def getParent(hook: ConnectionInfo): ConnectionInfo = {
        // Calculate where parent is, then execute jumps in optimal manner
        var traversalList: Seq[TraversalDirection] = List()

        // There's a couple of cases
        // case 1: hook is already parent (only works for node2)
        if (hook.nodeId.isParentOf(this.nodeId)) {
            return hook
        }

        // case 2: hook's parent is our parent (e.g. node3)
        if (hook.nodeId.getParentId.isParentOf(this.nodeId)) {
            traversalList = List(TraversalParent)
        }

        // case 3: hook's parent's right neighbour is our parent (e.g. node4, node6)
        if (hook.nodeId.getParentId.getRightNeighbourId.isParentOf(this.nodeId)) {
            traversalList = List(TraversalParent, TraversalRightNeighbour)
        }

        traverseByDirections(hook, traversalList)
    }

    def traverseByDirections(_node: ConnectionInfo, _traversalDirections: Seq[TraversalDirection]): ConnectionInfo = {
        var node = _node
        var traversalDirections = _traversalDirections

        while (traversalDirections.nonEmpty) {
            val nodeNeighbours = Node.getNeighboursFromNode(node)
            node = nodeNeighbours(traversalDirections.head)
            traversalDirections = traversalDirections.drop(1)
        }

        node
    }

    def tryStartGame(dimH: Int, dimW: Int, numGames: Int): Unit = {
        val numNodes = this.getExactNumNodes
        val gameToken = new GameToken(numGames, numNodes, dimH, dimW)

        // Send it around, start from us
        gameToken.sendToNext(this.asConnectionInfo)

        // Sleep 100ms, send status token around
        Thread.sleep(100)

        val statusToken = new StatusToken(gameToken.id, numGames)
        statusToken.sendToNext(this.asConnectionInfo)
    }

    def takeCareOfStatusToken(statusToken: StatusToken): Unit = {
        this.contextPrintln("Got StatusToken with gameTokenId: " + statusToken.gameTokenId)

        // TODO: Check if game already finished
        if (statusToken.percentComplete >= 1 - Constants.EPS) {
            this.contextPrintln("StatusToken finished for gameTokenId: " + statusToken.gameTokenId)
            return
        }

        // Check if 1st time visiting this node
        if (!this.statusTokenLastVisited.contains(statusToken.gameTokenId)) {
            // Set games taken last time from this node to 0
            this.statusTokenLastVisited(statusToken.gameTokenId) = 0
        }

        // TODO: Check if matches current gameToken

        // Update count (subtract what was added last time, and add current num of finished games)
        while (this.currentGameProcessor == null) {
            // If StatusToken was faster than GameToken, wait...
            Thread.sleep(100)
        }
        val gamesWonUntilNow = this.currentGameProcessor.gamesWonByX + this.currentGameProcessor.gamesWonByO + this.currentGameProcessor.gamesWonByNone
        statusToken.gamesComplete +=
            gamesWonUntilNow - statusTokenLastVisited(statusToken.gameTokenId)

        // Update num games accounted for
        this.statusTokenLastVisited(statusToken.gameTokenId) = gamesWonUntilNow
        this.currentGameToken.gamesFinished = statusToken.gamesComplete

        contextPrintln("StatusToken(" + statusToken.gameTokenId + ") complete: " + statusToken.percentComplete * 100 + "%")

        statusToken.sendToNext(this.getNeighbours(TraversalRightNeighbour))
    }

    def takeCareOfGameToken(gameToken: GameToken): Unit = {
        this.contextPrintln("Got GameToken with id: " + gameToken.id)

        // If token is empty, time to kill it...
        if (gameToken.gameBuckets.isEmpty) {
            gameToken.notifyPrevVisited()
            gameToken.notifyPrevVisited2()
            return
        }

        // Lock the state
        this.stateLock.synchronized {
            // Check if game already running
            if (this.state == Busy && this.currentGameToken.id != gameToken.id) {
                // There's a conflict, resolve it

                // 2 cases:
                // 1 - either currently running token was first, in which case we discard the received token
                // 2 - the received token was first, replace the running one

                // case 1 - we discard the received token
                if (gameToken.createdAt > this.currentGameToken.id) {
                    // Just confirm receipt and do nothing
                    gameToken.notifyPrevVisited2()
                    gameToken.notifyPrevVisited()

                    return
                } else {
                    // case 2 - we need to replace the already running token
                    // gameToken.createdAt < this.currentGameTokenId
                    // TODO: Replace token
                    ???
                }
            }

            // Set retries to 3
            this.tokenRetriesLeft = Constants.TOKEN_RETRIES_CNT

            // Startup: setup everything if gameToken visiting for the 1st time
            if (this.state == IdleNoWork) {
                this.state = Busy

                this.currentGameToken = gameToken
                this.currentGameProcessor = new GameProcessor(gameToken, this)
            }

            // If any games left, add them
            if (gameToken.gameBuckets.nonEmpty) {
                val gameBucket = gameToken.gameBuckets.pop()
                this.currentGameProcessor.addGamesFromRangesAndCount(gameBucket._1, gameBucket._2, gameBucket._3)
            }


            // Send token to next
            gameToken.shiftVisited(this.asConnectionInfo)
            gameToken.sendToNext(this.getNeighbours(TraversalRightNeighbour))

            // Notify prev2 we got the token, don't care if fails at this point (node might've gone down)
            gameToken.notifyPrevVisited2()

            // Thread that checks whether sending succeeded
            new Thread(() => {
                val node: Node = this

                while (node.tokenRetriesLeft > 0) {
                    // Sleep, check if managed to send
                    Thread.sleep(Constants.TOKEN_RETRY_MS)

                    // Could use a lock...
                    if (node.tokenRetriesLeft > 0) {
                        node.currentGameToken.sendToNext(node.getNeighbours(TraversalRightNeighbour))
                    }

                    tokenRetriesLeft -= 1
                }
            }).start()
        }
    }

    def storeSnapshotWithNeighbours(): Unit = {
        val selfSnapshot: NodeSnapshot = Node.createSnapshot(this)

        this.neighbours.foreach {
            case (_, neighbourConnInfo) =>
                val (neighbourSocket, in, out) = neighbourConnInfo.connect
                out.writeObject("want_store_snapshot")
                out.writeObject(this.asConnectionInfo)
                out.writeObject(selfSnapshot)
                out.flush()

                neighbourSocket.close()
        }
    }

    def addSnapshot(nodeConnInfo: ConnectionInfo, nodeSnapshot: NodeSnapshot): Unit = {
        val traversalOpt: Option[TraversalDirection] = this.nodeId.getTraversalTo(nodeConnInfo.nodeId)
        if (traversalOpt.isEmpty) {
            this.contextPrintln("WTF: " + nodeConnInfo.toString)
            throw new Exception("How df is traversal opt empty?")
        }

        this.snapshots(traversalOpt.get) = nodeSnapshot
    }

    def getSnapshot(traversalDirection: TraversalDirection): NodeSnapshot = {
        this.snapshots(traversalDirection)
    }

    def getLastNode: ConnectionInfo = {
        var currConnInfo = this.asConnectionInfo
        var currNeighbours = this.getNeighbours

        while (currConnInfo.nodeId.id != 1) {
            currConnInfo = currNeighbours(TraversalParent)
            currNeighbours = Node.getNeighboursFromNode(currConnInfo)
        }

        // We got node1, return its left neighbour, the last node
        currNeighbours(TraversalLeftNeighbour) // Return last node in graph
    }

    def getExactNumNodes: Int = {
        this.getLastNode.nodeId.id
    }

    def getEstimateNumNodes_RightEdge = {
        throw new NotImplementedError("implement traversal to the rightmost-then-lowest edge")
    }

    def tryReserve: Boolean = {
        stateLock.synchronized {
            if (this.neighbours.contains(TraversalRightNeighbour)) {
                return false
            }

            // Set state to in reconstruction
            this.state = InReconstruction
            return true
        }
    }

    def reposition(leftNeighbour: ConnectionInfo): Unit = {
        val (socket, in, out) = leftNeighbour.connect

        out.writeObject("want_snapshot")
        out.writeObject(TraversalRightNeighbour)
        out.flush()

        val myNewSnapshot = in.readObject().asInstanceOf[NodeSnapshot]

        // Notify of my removal
        this.notifyNewNeighbour(this.neighbours(TraversalLeftNeighbour), this.neighbours(TraversalRightNeighbour)) // left neighbour
        this.notifyNewNeighbour(this.neighbours(TraversalParent), null) // parent

        // Update self from snapshot
        this.nodeId = myNewSnapshot.nodeId
        this.replaceNeighbours(myNewSnapshot.neighbours)
        this.currentGameProcessor = currentGameProcessor.mergeWith(myNewSnapshot.gameProcessor)
        this.currentGameToken = myNewSnapshot.gameToken

        // Notify my neighbours of me
        notifyAllNewNeighbours()

        this.stateLock.synchronized {
            this.currentGameProcessor match {
                case cgp if cgp == null => this.state = IdleNoWork
                case cgp if cgp.gamesToPlay.nonEmpty => this.state = Busy
                case cgp if cgp.gamesToPlay.isEmpty && cgp.gamesWonByX + cgp.gamesWonByO + cgp.gamesWonByNone > 0 => this.state = IdleHasWork
            }
        }

        socket.close()
    }

    def replaceNeighbours(newNeighbours: ConcurrentMapRef): Unit = {
        this.neighboursLock.synchronized {
            this.neighbours = newNeighbours
        }
    }

    def notifyAllNewNeighbours(): Unit = {
        this.neighboursLock.synchronized {
            this.neighbours.foreach {
                case (traversalDir, connInfo) =>
                    notifyNewNeighbour(connInfo, this.asConnectionInfo)
            }
        }
    }

    def notifyNewNeighbour(neighbourConn: ConnectionInfo, newConn: ConnectionInfo): Unit = {
        val (socket, in, out) = neighbourConn.connect
        out.writeObject("notify_new_neighbour")
        out.writeObject(newConn)
        out.flush()

        socket.close()
    }

//    // Whether notify succeeded
//    def notifyBootstrap(): Boolean = {
//        val (bootstrapSocket, bsIn, bsOut) = new ConnectionInfo(Constants.BOOTSTRAP_IP_ADDRESS, Constants.BOOTSTRAP_SERVER_PORT, NodeId.default).connect
//        bsOut.writeObject("add_me_blok_sam")
//        bsOut.writeObject(this.asConnectionInfo)
//
//        val bsResp = bsIn.readObject().asInstanceOf[String]
//        bootstrapSocket.close()
//
//        bsResp == "add_ack"
//    }
//
//    def tryJoin(nodeToJoin: ConnectionInfo, traversalDirection: TraversalDirection): Boolean = {
//        val (socket, in, out) = nodeToJoin.connect
//
//        out.writeObject("join")
//
//        // Set correct node id
//        this.nodeId = nodeToJoin.nodeId.getRightNeighbourId
//        out.writeObject(this.asConnectionInfo)
//
//        val resp = in.readObject().asInstanceOf[String]
//
//        if (resp != "join_accepted") {
//            // Revert to default node id
//            this.nodeId = NodeId.default
//
//            socket.close()
//            return false
//        }
//
//        // resp == "join_accepted"
//        out.writeObject("join_ack")
//        out.flush()
//
//        val traversalDirToNodeToJoinOpt = this.nodeId.getTraversalTo(nodeToJoin.nodeId)
//        this.addNeighbour(traversalDirToNodeToJoinOpt.get, nodeToJoin)
//
//        socket.close()
//        true
//    }
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



    def getNeighbours: Map[TraversalDirection, ConnectionInfo] = this.neighbours.clone().toMap

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

        if (neighbour.nodeId.id != this.nodeId.id) {
            // Update snapshot
//            this.storeSnapshotWithNeighbours()
        }

        printNeighbours()
    }

    def printNeighbours(): Unit = this.contextPrintln("Updated neighbours:\n" + this.neighbours.toString)

    def contextPrintln(string: String): Unit = Node.contextPrintln(this, string)

    def main(args: Array[String]): Unit = {
        val node = new Node("")

        // Start the node
        new Thread(node).start()

        while (true) {
            val input = scala.io.StdIn.readLine("connect_five> ")
            input match {
                case "status" =>
                    ???
                case "new game" =>
                    val dimH = scala.io.StdIn.readInt()
                    val dimW = scala.io.StdIn.readInt()
                    val numGames = scala.io.StdIn.readInt()

                    this.tryStartGame(dimH, dimW, numGames)
                case _ =>
                    println(
                        "Commands:" +
                        "\n1 - new game" +
                        "\n    [dimH, dimW, numGames]" +
                        "\n2 - status" +
                        "\n"
                    )
            }
        }
    }
}

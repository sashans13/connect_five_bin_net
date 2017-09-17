import common.{RunMode, ThreadMode}
import node.Node

object Playground {
    def main(args: Array[String]): Unit = {

        val runMode: RunMode = ThreadMode

        // Start in thread mode
        bootstrap.Bootstrap.startServer(runMode)

        val numNodesToLaunch = 2

        for (i <- 0 until numNodesToLaunch) {
            new Thread(new Node).start()
        }

//        val node1 = new Node
//        val node2 = new Node
//        val node3 = new Node
//        val node4 = new Node
//
//        node1.startNode()
//        node2.startNode()
//        node3.startNode()
//        node4.startNode()
    }
}

package node

import game.GameProcessor
import tokens.GameToken

class NodeSnapshot extends Serializable {
    var nodeId: NodeId = _
    var neighbours: Node.ConcurrentMapRef = _
    var gameToken: GameToken = _
    var gameProcessor: GameProcessor = _
}

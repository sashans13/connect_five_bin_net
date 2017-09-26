package node

sealed abstract class TraversalDirection(val dir: Int) extends Serializable

case object TraversalSelf           extends TraversalDirection(dir = -1)
case object TraversalLeftChild      extends TraversalDirection(dir = 0)
case object TraversalRightChild     extends TraversalDirection(dir = 1)
case object TraversalRightNeighbour extends TraversalDirection(dir = 2)
case object TraversalParent         extends TraversalDirection(dir = 3)
case object TraversalLeftNeighbour  extends TraversalDirection(dir = 4)

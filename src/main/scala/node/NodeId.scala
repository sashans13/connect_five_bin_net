package node

import common.Util

object NodeId {
    def default = NodeId(-1)
}

case class NodeId(id: Integer) {
    // If is power of two, it's on the left edge
    val isLeftEdgeInGraph: Boolean = Util.isPowerOfTwo(this.id)
    // If +1 is power of two, it's on the right edge (works for node 1 too)
    val isRightEdgeInGraph: Boolean = Util.isPowerOfTwo(this.id + 1)

    // Indexed from 1 => node1 is depth1, node2 and node3 are depth2
    def depth: Int = Util.log2floor(this.id) + 1

    def isLeftNeighbourOf(other: NodeId) = this.id == other.id - 1

    def isRightNeighbourOf(other: NodeId) = other.id + 1 == this.id

    def isLeftChildOf(other: NodeId) = other.id * 2 == this.id

    def isRightChildOf(other: NodeId) = other.id * 2 + 1 == this.id

    def isParentOf(other: NodeId) = other.id / 2 == this.id

    def isOtherLeftNeighbour(other: NodeId) = other.id + 1 == this.id || (this.id == 1)

    def isOtherRightNeighbour(other: NodeId) = other.id - 1 == this.id || (other.id == 1)

    def isOtherLeftChild(other: NodeId) = this.id * 2 == other.id

    def isOtherRightChild(other: NodeId) = this.id * 2 + 1 == other.id

    def isOtherParent(other: NodeId) = this.id / 2 == other.id

    def getRightNeighbourId: NodeId = NodeId(this.id + 1)

    def getLeftChildId: NodeId = NodeId(this.id * 2)

    def getParentId: NodeId = NodeId(this.id / 2)


    def getTraversalTo(other: NodeId): Option[TraversalDirection] = {
        // TODO: Sometimes more than 1 can be true!!!
        // TODO: For example, for node1, node2 is rightneighbour and leftchild
        var traversalDirOpt: Option[TraversalDirection] = None

        if (this.isOtherParent(other)) {
            traversalDirOpt = Option(TraversalParent)
        } else if (this.isOtherLeftChild(other)) {
            traversalDirOpt = Option(TraversalLeftChild)
        } else if (this.isOtherRightChild(other)) {
            traversalDirOpt = Option(TraversalRightChild)
        } else if (this.isOtherRightNeighbour(other)) {
            traversalDirOpt = Option(TraversalRightNeighbour)
        } else if (this.isOtherLeftNeighbour(other)) {
            traversalDirOpt = Option(TraversalLeftNeighbour)
        }

        traversalDirOpt
    }

    override def toString: String = "NodeId(" + (if (this.id == -1) "default" else this.id) + ")"
}
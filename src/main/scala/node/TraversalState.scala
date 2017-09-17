package node

sealed trait TraversalState

case object TraversedRight extends TraversalState

/**
  * Didn't traverse anywhere
  */
case object NoTraversal extends TraversalState

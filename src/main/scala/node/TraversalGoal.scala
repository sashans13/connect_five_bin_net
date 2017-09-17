package node

sealed trait TraversalGoal

case object FindJoinableNode extends TraversalGoal

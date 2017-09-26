package node

sealed trait NodeState

/**
  * Created but isn't connected yet
  */
case object Initial extends NodeState

/**
  * Currently awaiting work
  */
case object IdleNoWork extends NodeState

/**
  * Not working but has a GameProcessor
  */
case object IdleHasWork extends NodeState

/**
  * Currently working
  */
case object Busy extends NodeState

/**
  * Architecture changing
  */
case object InReconstruction extends NodeState
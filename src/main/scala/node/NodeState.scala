package node

sealed trait NodeState

/**
  * Created but isn't connected yet
  */
case object Initial extends NodeState

/**
  * Currently connected
  */
case object Connected extends NodeState

/**
  * Architecture changing, wait until state changes to Connected
  */
case object InReconstruction extends NodeState
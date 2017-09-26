package game

sealed abstract class GameWinner extends Serializable

case object GameWinnerX extends GameWinner
case object GameWinnerO extends GameWinner
case object GameWinnerNone extends GameWinner

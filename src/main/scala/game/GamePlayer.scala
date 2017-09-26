package game

sealed abstract class GamePlayer(val id: Int) extends Serializable

case object GamePlayerX extends GamePlayer(id = 0)
case object GamePlayerO extends GamePlayer(id = 1)

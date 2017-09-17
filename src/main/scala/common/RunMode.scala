package common

sealed trait RunMode

case object ThreadMode extends RunMode
case object ProcessMode extends RunMode
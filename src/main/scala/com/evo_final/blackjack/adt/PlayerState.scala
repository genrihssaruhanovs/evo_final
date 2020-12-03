package com.evo_final.blackjack.adt

sealed trait PlayerState

case object IsInGame extends PlayerState
case object TurnDone extends PlayerState
case object TurnNow extends PlayerState
case object BetPlaced extends PlayerState
case object Surrendered extends PlayerState
case object Insured extends PlayerState

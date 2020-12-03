package com.evo_final.blackjack.adt

sealed trait PlayerDecision

case object Hit extends PlayerDecision
case object Stand extends PlayerDecision
case object DoubleDown extends PlayerDecision
case object Surrender extends PlayerDecision
case object Split extends PlayerDecision
case object Insurance extends PlayerDecision

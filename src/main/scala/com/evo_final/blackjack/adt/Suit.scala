package com.evo_final.blackjack.adt

sealed trait Suit {
  def character: Char
  override def toString: String = character.toString
}

case object Hearts extends Suit {
  val character = 'h'
}

case object Diamonds extends Suit {
  val character = 'd'
}

case object Clubs extends Suit {
  val character = 'c'
}

case object Spades extends Suit {
  val character = 's'
}

object Suit {
  def allSuits: List[Suit] = Hearts :: Diamonds :: Clubs :: Spades :: Nil
}

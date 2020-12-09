package com.evo_final.blackjack.cards

sealed trait Suit {
  def character: Char
  override def toString: String = character.toString
}

object Suit {
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
  def allSuits: List[Suit] = List(Hearts, Diamonds, Clubs, Spades)
}

package com.evo_final.blackjack.cards

sealed trait Rank {
  def character: Char
  override def toString: String = character.toString
  def strength: Int =
    toString.toIntOption match {
      case Some(v)                  => v
      case None if character == 'A' => 11
      case _                        => 10
    }
}

object Rank {
  case object Two extends Rank { val character = '2' }
  case object Three extends Rank { val character = '3' }
  case object Four extends Rank { val character = '4' }
  case object Five extends Rank { val character = '5' }
  case object Six extends Rank { val character = '6' }
  case object Seven extends Rank { val character = '7' }
  case object Eight extends Rank { val character = '8' }
  case object Nine extends Rank { val character = '9' }
  case object Ten extends Rank { val character = 'T' }
  case object Jack extends Rank { val character = 'J' }
  case object Queen extends Rank { val character = 'Q' }
  case object King extends Rank { val character = 'K' }
  case object Ace extends Rank { val character = 'A' }

  def allRanks: List[Rank] =
    List(
      Two,
      Three,
      Four,
      Five,
      Six,
      Seven,
      Eight,
      Nine,
      Ten,
      Jack,
      Queen,
      King,
      Ace,
    )
}

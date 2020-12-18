package com.evo_final.blackjack.server.output

import com.evo_final.blackjack.Amount
import com.evo_final.blackjack.game_logic.PossibleActions

sealed trait ToClient

object ToClient {
  case class Communication(possibleActions: List[PossibleActions], text: String) extends ToClient
  case class Message(text: String) extends ToClient
  case class RoundState(
    otherPlayers: List[OtherPlayer],
    currentPlayer: CurrentPlayer,
    dealer: Dealer
  ) extends ToClient

  case class Result(
    message: String,
    amountBack: Amount
  ) extends ToClient
}

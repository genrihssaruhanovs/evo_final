package com.evo_final.blackjack.server.output

import com.evo_final.blackjack.Amount
import com.evo_final.blackjack.game_logic.adt.PossibleActions

sealed trait ToClient

object ToClient {
  case class Communication(possibleActions: List[PossibleActions], text: String) extends ToClient
  case class Message(text: MessageToClient) extends ToClient
  case class Balance(balance: Amount, message: MessageToClient) extends ToClient
  case class RoundState(
    otherPlayers: List[OtherPlayer],
    currentPlayer: CurrentPlayer,
    dealer: Dealer
  ) extends ToClient
}

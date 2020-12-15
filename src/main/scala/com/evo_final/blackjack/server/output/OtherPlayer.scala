package com.evo_final.blackjack.server.output

import com.evo_final.blackjack.PlayerId
import com.evo_final.blackjack.cards.Card
import com.evo_final.blackjack.game_logic.PlayerState.{TurnDone, TurnNow}
import com.evo_final.blackjack.game_logic.{Player, PlayerState}

case class OtherPlayer(
  cards: List[Card],
  score: Int,
  state: Option[PlayerState]
)

object OtherPlayer {
  def of(player: Player): OtherPlayer = {
    OtherPlayer(
      player.hand.cards,
      player.hand.score,
      if (player.states.contains(TurnDone)) Some(TurnDone)
      else if (player.states.contains(TurnNow)) Some(TurnNow)
      else None
    )
  }
  def many(players: Map[PlayerId, Player], currentPlayerId: PlayerId): List[OtherPlayer] = {
    players.collect {
      case (id, player) if id != currentPlayerId =>
        OtherPlayer(
          player.hand.cards,
          player.hand.score,
          if (player.states.contains(TurnDone)) Some(TurnDone)
          else if (player.states.contains(TurnNow)) Some(TurnNow)
          else None
        )
    }.toList
  }
}

package com.evo_final.blackjack

import com.evo_final.blackjack.game_logic.Round

case class Game(
  roundInProcess: Boolean,
  round: Round,
  clients: Any = ??? // will be some map between connection and Player Id
) {}

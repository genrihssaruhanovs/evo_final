package com.evo_final.blackjack.server

import com.evo_final.blackjack.cards.Card
import com.evo_final.blackjack.game_logic.{PlayerDecision, PlayerState, PossibleActions}
import com.evo_final.blackjack.server.FromClient._
import com.evo_final.blackjack.server.output.ToClient._
import com.evo_final.blackjack.server.output._
import io.circe.generic.extras.semiauto.{deriveEnumerationCodec, deriveEnumerationDecoder}
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder, Json}

object JsonCodecs {
  //encoders
  implicit val cardEncoder: Encoder[Card] = card => Json.fromString(card.toString)
  implicit val possibleActionsEncoder: Encoder[PossibleActions] = deriveEnumerationCodec[PossibleActions]
  implicit val playerStateEncoder: Encoder[PlayerState] = deriveEnumerationCodec[PlayerState]
  implicit val dealerEncoder: Encoder[Dealer] = deriveEncoder[Dealer]
  implicit val currentPlayerEncoder: Encoder[CurrentPlayer] = deriveEncoder[CurrentPlayer]
  implicit val otherPlayerEncoder: Encoder[OtherPlayer] = deriveEncoder[OtherPlayer]
  implicit val messageEncoder: Encoder[Message] = deriveEncoder[Message]
  implicit val communicationEncoder: Encoder[Communication] = deriveEncoder[Communication]
  implicit val roundStateEncoder: Encoder[RoundState] = deriveEncoder[RoundState]
  implicit val resultDecoder: Encoder[Result] = deriveEncoder[Result]
  implicit val toClientDecoder: Encoder[ToClient] = deriveEncoder[ToClient]

  //decoders
  implicit val playerDecisionDecoder: Decoder[PlayerDecision] = deriveEnumerationDecoder[PlayerDecision]
  implicit val betDecoder: Decoder[Bet] = deriveDecoder[Bet]
  implicit val actionDecoder: Decoder[Action] = deriveDecoder[Action]
  implicit val fromClientDecoder: Decoder[FromClient] = deriveDecoder[FromClient]

}

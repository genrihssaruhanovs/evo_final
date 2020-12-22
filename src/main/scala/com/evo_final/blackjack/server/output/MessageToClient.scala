package com.evo_final.blackjack.server.output

sealed trait MessageToClient

object MessageToClient {
  final case object BetFailure extends MessageToClient {
    override def toString: String = "Issue when placing bet"
  }
  final case object DecisionFailure extends MessageToClient {
    override def toString: String = "Decision couldn't be executed"
  }
  final case object RequestFailure extends MessageToClient {
    override def toString: String = "Wrong request"
  }
  final case object BetAcceptedWait extends MessageToClient {
    override def toString: String = "Bet accepted, wait till others place their bet"
  }
  final case object InsufficientBalance extends MessageToClient {
    override def toString: String = "Not enough balance"
  }
  final case object InternalFailure extends MessageToClient {
    override def toString: String = "Internal error"
  }
  final case object TopUpFailure extends MessageToClient {
    override def toString: String = "Top up failure"
  }
  final case object BetIsNull extends MessageToClient {
    override def toString: String = "Bet cannot be 0"
  }
  final case object TopUpIsNull extends MessageToClient {
    override def toString: String = "Bet cannot be 0"
  }
  final case object RoundInProgress extends MessageToClient {
    override def toString: String = "Round in progress, please wait"
  }
  final case object InconsistentRequest extends MessageToClient {
    override def toString: String = "Request parsing error"
  }
  final case object BetAccepted extends MessageToClient {
    override def toString: String = "Bet accepted"
  }
  final case object TopUpSuccess extends MessageToClient {
    override def toString: String = "Topped up"
  }
  final case object MessageUnit extends MessageToClient {
    override def toString: String = ""
  }
  final case object BetTimeout extends MessageToClient {
    override def toString: String = "Bet step timeout, please wait till the round ends"
  }
}

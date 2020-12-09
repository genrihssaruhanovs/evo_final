package com.evo_final.blackjack.game_logic

import com.evo_final.blackjack.cards.Card
import com.evo_final.blackjack.cards.Rank._
import com.evo_final.blackjack.cards.Suit._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class HandSpec extends AnyFreeSpec with Matchers {
  "Hand evaluation should" - {
    val ace = Card(Ace, Hearts)
    val queen = Card(Queen, Hearts)
    val three = Card(Three, Hearts)
    val eight = Card(Eight, Hearts)
    "provide correct hand value" in {
      PlayerHand(List(queen, three)).score shouldEqual 13
      PlayerHand(List(queen, ace)).score shouldEqual 21
      PlayerHand(List(ace, three)).score shouldEqual 14
      PlayerHand(List(ace, ace)).score shouldEqual 12
      PlayerHand(List(ace, queen, ace)).score shouldEqual 12
      PlayerHand(List(queen, ace, queen, ace)).score shouldEqual 22
    }
    "set 'blackjack' correctly" in {
      PlayerHand(List(ace, queen)).isBlackJack shouldEqual true
      PlayerHand(List(ace, three)).isBlackJack shouldEqual false
      PlayerHand(List(ace, three, eight)).isBlackJack shouldEqual false
    }
    "set 'bust' correctly" in {
      PlayerHand(List(ace, queen)).isBust shouldEqual false
      PlayerHand(List(ace, three, eight)).isBust shouldEqual false
      PlayerHand(List(queen, three, queen)).isBust shouldEqual true
    }
    "work the same for dealer and player" in {
      PlayerHand(List(queen, three)).score shouldEqual DealerHand(List(queen, three)).score
      PlayerHand(List(queen, ace)).score shouldEqual DealerHand(List(queen, ace)).score
      PlayerHand(List(ace, three)).score shouldEqual DealerHand(List(ace, three)).score
      PlayerHand(List(ace, ace)).score shouldEqual DealerHand(List(ace, ace)).score
      PlayerHand(List(ace, queen, ace)).score shouldEqual DealerHand(List(ace, queen, ace)).score
      PlayerHand(List(queen, ace, queen, ace)).score shouldEqual DealerHand(
        List(queen, ace, queen, ace)
      ).score

      PlayerHand(List(ace, three, eight)).isBust shouldEqual DealerHand(List(ace, three, eight)).isBust
      PlayerHand(List(queen, three, queen)).isBust shouldEqual DealerHand(List(queen, three, queen)).isBust

      PlayerHand(List(ace, queen)).isBlackJack shouldEqual DealerHand(List(ace, queen)).isBlackJack
      PlayerHand(List(ace, three)).isBlackJack shouldEqual DealerHand(List(ace, three)).isBlackJack
    }
  }
}

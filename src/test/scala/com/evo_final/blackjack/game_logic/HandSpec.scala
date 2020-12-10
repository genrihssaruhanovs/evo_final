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
      Hand(List(queen, three)).score shouldEqual 13
      Hand(List(queen, ace)).score shouldEqual 21
      Hand(List(ace, three)).score shouldEqual 14
      Hand(List(ace, ace)).score shouldEqual 12
      Hand(List(ace, queen, ace)).score shouldEqual 12
      Hand(List(queen, ace, queen, ace)).score shouldEqual 22
    }
    "set 'blackjack' correctly" in {
      Hand(List(ace, queen)).isBlackJack shouldEqual true
      Hand(List(ace, three)).isBlackJack shouldEqual false
      Hand(List(ace, three, eight)).isBlackJack shouldEqual false
    }
    "set 'bust' correctly" in {
      Hand(List(ace, queen)).isBust shouldEqual false
      Hand(List(ace, three, eight)).isBust shouldEqual false
      Hand(List(queen, three, queen)).isBust shouldEqual true
    }
  }
}

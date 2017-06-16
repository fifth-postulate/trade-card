module TradeCard.Client exposing (main)


import Html
import TradeCard.Card as Card


main = Card.view card

card : Card.Card
card = { id = 15 }

module TradeCard.Card exposing (Card, view)


import Html
import Html.Attributes as Attribute


type alias Card =
    {
      id: Int
    }


view : Card -> Html.Html msg
view card =
    let
        cardId : String
        cardId = "card-" ++ (toString card.id)
    in
        Html.div
            [ Attribute.classList [ ("card", True), (cardId, True) ] ]
            [
              Html.span [] [ Html.text (cardFace card) ]
            ]


cardFace : Card -> String
cardFace card =
    leftPad (toString card.id) 3 "0"


leftPad : String -> Int -> String -> String
leftPad original target padding =
    if (String.length original) < target then
        leftPad (padding ++ original) target padding
    else
        original

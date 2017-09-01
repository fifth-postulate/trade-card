module TradeCard.Card exposing (Card, view)

import Html
import Html.Attributes as Attribute
import Html.Events as Event


type alias Card =
    { id : Int
    }


view : (Card -> msg) -> Card -> Html.Html msg
view message card =
    let
        cardId : String
        cardId =
            "card-" ++ (toString card.id)
    in
        Html.div
            [ Attribute.classList [ ( "card", True ), ( cardId, True ) ]
            , Event.onClick (message card)
            ]
            [ Html.span [] [ Html.text (cardFace card) ]
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

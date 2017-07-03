module TradeCard.View exposing (collectionView)


import Html
import Html.Attributes as Attribute
import TradeCard.Card as Card
import TradeCard.Collection as Collection


collectionView : (Card.Card -> msg) -> (Card.Card -> msg) -> (Card.Card -> msg) ->  Collection.Collection -> Html.Html msg
collectionView collectedMessage doubleMessage missingMessage collection =
    Html.div
        [ Attribute.class "card collection" ]
        [
          viewCardList collectedMessage (Collection.collected collection)
        , duplicityList doubleMessage (Collection.doubles collection)
        , viewCardList missingMessage (Collection.missing collection)
        ]


viewCardList : (Card.Card -> msg) -> List Card.Card -> Html.Html msg
viewCardList message cards =
    Html.div
        [ Attribute.class "card list" ]
        (List.map (viewCard message) cards)


viewCard : (Card.Card -> msg) -> Card.Card -> Html.Html msg
viewCard message card =
    (Card.view message) card

duplicityList : (Card.Card -> msg) -> List (Card.Card, Int) -> Html.Html msg
duplicityList message doubles =
    Html.div
        [ Attribute.class "card duplicity list"]
        (List.map (doublicityView message) doubles)


doublicityView : (Card.Card -> msg) -> (Card.Card, Int) -> Html.Html msg
doublicityView message (card, duplicity) =
    Html.div
        []
        [
          Card.view message card
        , Html.span [ Attribute.class "duplicity" ] [ Html.text (toString duplicity) ]
        ]




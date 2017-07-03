module TradeCard.View exposing (collectionView)


import Html
import Html.Attributes as Attribute
import Html.Events as Event
import TradeCard.Card as Card
import TradeCard.Collection as Collection


collectionView : (Card.Card -> msg) -> (Card.Card -> msg) -> (Card.Card -> msg) -> (Card.Card -> msg) ->  Collection.Collection -> Html.Html msg
collectionView collectedMessage lostMessage doubleMessage missingMessage collection =
    Html.div
        [ Attribute.class "card collection" ]
        [
          viewCardList collectedMessage lostMessage (Collection.collected collection)
        , duplicityList doubleMessage (Collection.doubles collection)
        , viewCardList missingMessage lostMessage (Collection.missing collection)
        ]


viewCardList : (Card.Card -> msg) -> (Card.Card -> msg)-> List Card.Card -> Html.Html msg
viewCardList primary secondary cards =
    Html.div
        [ Attribute.class "card list" ]
        (List.map (viewCard primary secondary) cards)


viewCard : (Card.Card -> msg) -> (Card.Card -> msg) -> Card.Card -> Html.Html msg
viewCard primary secondary card =
    Html.div
        []
        [
          (Card.view primary) card
        , Html.span [ Event.onClick (secondary card) ] [ Html.text "x" ]
        ]


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




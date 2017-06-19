module TradeCard.View exposing (collectionView)


import Html
import Html.Attributes as Attribute
import TradeCard.Card as Card
import TradeCard.Collection as Collection


collectionView : Collection.Collection -> Html.Html msg
collectionView collection =
    Html.div
        [ Attribute.class "card collection" ]
        [
          viewCardList (Collection.collected collection)
        , duplicityList (Collection.doubles collection)
        , viewCardList (Collection.missing collection)
        ]


viewCardList : List Card.Card -> Html.Html msg
viewCardList cards =
    Html.div
        [ Attribute.class "card list" ]
        (List.map Card.view cards)


duplicityList : List (Card.Card, Int) -> Html.Html msg
duplicityList doubles =
    Html.div
        [ Attribute.class "card duplicity list"]
        (List.map doublicityView doubles)


doublicityView : (Card.Card, Int) -> Html.Html msg
doublicityView (card, duplicity) =
    Html.div
        []
        [
          Card.view card
        , Html.span [ Attribute.class "duplicity" ] [ Html.text (toString duplicity) ]
        ]




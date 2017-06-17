module TradeCard.Collection exposing (Collection, empty, collect, remove, view, CollectError)


import Dict
import Html
import Html.Attributes as Attribute
import TradeCard.Card as Card


type alias Collection =
    {
      range: (Int, Int)
    , collected: Dict.Dict Int (Card.Card, Int)
    }

empty : Int -> Int -> Collection
empty low high =
    {
      range = ((min low high), (max low high))
    , collected = Dict.empty
    }


collect : Card.Card -> Collection -> Result CollectError Collection
collect card collection =
    let
        id = card.id

        range = collection.range

        idInRange = (Tuple.first range) <= id && id <= (Tuple.second range)
    in
        if idInRange then
            case Dict.get id collection.collected of
                Just (aCard, count) ->
                    let
                        newlyCollected =
                            Dict.insert id (aCard, count + 1) collection.collected
                    in
                        Ok { collection | collected = newlyCollected }

                Nothing ->
                    let newlyCollected =
                            Dict.insert id (card, 1) collection.collected
                    in
                        Ok { collection | collected = newlyCollected }
        else
            (Err (OutsideRange range id))


type CollectError =
    OutsideRange (Int, Int) Int


remove : Card.Card -> Collection -> Collection
remove card collection =
    let
        newlyCollected =
            Dict.remove card.id collection.collected
    in
        { collection | collected = newlyCollected }


view : Collection -> Html.Html msg
view collection =
    Html.div
        [ Attribute.class "card collection" ]
        [
         viewCardList (all collection)
        ]


viewCardList : List Card.Card -> Html.Html msg
viewCardList cards =
    Html.div
        [ Attribute.class "card list" ]
        (List.map Card.view cards)


all : Collection -> List Card.Card
all collection =
    collection.collected |>
    Dict.values  |>
    (List.map Tuple.first)

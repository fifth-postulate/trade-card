module TradeCard.Collection exposing (Collection, empty, collect, remove, collected, doubles, missing, CollectError)

import Dict
import TradeCard.Card as Card


type alias Collection =
    { range : ( Int, Int )
    , collected : Dict.Dict Int ( Card.Card, Int )
    }


empty : Int -> Int -> Collection
empty low high =
    { range = ( (min low high), (max low high) )
    , collected = Dict.empty
    }


collect : Card.Card -> Collection -> Result CollectError Collection
collect card collection =
    let
        id =
            card.id

        range =
            collection.range

        idInRange =
            (Tuple.first range) <= id && id <= (Tuple.second range)
    in
        if idInRange then
            case Dict.get id collection.collected of
                Just ( aCard, count ) ->
                    let
                        newlyCollected =
                            Dict.insert id ( aCard, count + 1 ) collection.collected
                    in
                        Ok { collection | collected = newlyCollected }

                Nothing ->
                    let
                        newlyCollected =
                            Dict.insert id ( card, 1 ) collection.collected
                    in
                        Ok { collection | collected = newlyCollected }
        else
            Err (OutsideRange range id)


type CollectError
    = OutsideRange ( Int, Int ) Int


remove : Card.Card -> Collection -> Collection
remove card collection =
    case Dict.get card.id collection.collected of
        Just ( card, count ) ->
            let
                newlyCollected =
                    if count > 1 then
                        Dict.insert card.id ( card, count - 1 ) collection.collected
                    else
                        Dict.remove card.id collection.collected
            in
                { collection | collected = newlyCollected }

        Nothing ->
            collection


collected : Collection -> List Card.Card
collected collection =
    collection.collected
        |> Dict.values
        |> (List.map Tuple.first)


doubles : Collection -> List ( Card.Card, Int )
doubles collection =
    collection.collected
        |> Dict.values
        |> List.filter (\t -> (Tuple.second t) > 1)
        |> List.map (\t -> ( Tuple.first t, (Tuple.second t) - 1 ))


missing : Collection -> List Card.Card
missing collection =
    collection.range
        |> uncurry List.range
        |> List.filter (negate (isCollected collection))
        |> List.map (\id -> { id = id })


isCollected : Collection -> Int -> Bool
isCollected collection id =
    Dict.member id collection.collected


negate : (a -> Bool) -> a -> Bool
negate predicate value =
    not (predicate value)

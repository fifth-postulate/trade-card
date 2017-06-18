module TradeCard.Client exposing (main)

import Html
import TradeCard.Card as Card
import TradeCard.Collection as Collection
import TradeCard.Market as Market

main : Html.Html msg
main =
    let
        trade = Market.trade collectionA collectionB

        aCards = Tuple.first trade

        bCards = Tuple.second trade
    in
        Html.div
            []
            (List.concat
                 [
                  [ Collection.view collectionA ]
                 , (List.map Card.view aCards)
                 , (List.map Card.view bCards)
                 ])


collectionA : Collection.Collection
collectionA =
    let
        result : Result Collection.CollectError Collection.Collection
        result =
               (Collection.empty 1 15)
            |> Ok
            |> Result.andThen (Collection.collect { id = 4 })
            |> Result.andThen (Collection.collect { id = 4 })
            |> Result.andThen (Collection.collect { id = 4 })
            |> Result.andThen (Collection.collect { id = 7 })
            |> Result.andThen (Collection.collect { id = 11 })
            |> Result.andThen (Collection.collect { id = 11 })
            |> Result.andThen (Collection.collect { id = 13 })
            |> Result.andThen (Collection.collect { id = 13 })
    in
        case result of
            Ok c->
                c

            Err _ ->
                Collection.empty 1 15


collectionB : Collection.Collection
collectionB =
    let
        result : Result Collection.CollectError Collection.Collection
        result =
               (Collection.empty 1 15)
            |> Ok
            |> Result.andThen (Collection.collect { id = 2 })
            |> Result.andThen (Collection.collect { id = 2 })
            |> Result.andThen (Collection.collect { id = 7 })
            |> Result.andThen (Collection.collect { id = 12 })
            |> Result.andThen (Collection.collect { id = 12 })
    in
        case result of
            Ok c->
                c

            Err _ ->
                Collection.empty 1 15

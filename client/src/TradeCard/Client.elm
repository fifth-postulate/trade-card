module TradeCard.Client exposing (main)

import Html
import TradeCard.Collection as Collection

main : Html.Html msg
main = Collection.view collection

collection : Collection.Collection
collection =
    let
        result : Result Collection.CollectError Collection.Collection
        result =
               (Collection.empty 1 15)
            |> Ok
            |> Result.andThen (Collection.collect { id = 4 })
            |> Result.andThen (Collection.collect { id = 7 })
            |> Result.andThen (Collection.collect { id = 11 })
            |> Result.andThen (Collection.collect { id = 13 })
    in
        (case result of
            Ok c->
             c

            Err _ ->
             Collection.empty 1 15)

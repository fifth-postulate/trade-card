module TradeCard.Market exposing (trade)

import TradeCard.Card as Card
import TradeCard.Collection as Collection


trade : Collection.Collection -> Collection.Collection -> ( List Card.Card, List Card.Card )
trade collectionA collectionB =
    let
        doublesA =
            collectionA
                |> Collection.doubles
                |> List.map Tuple.first

        missingA =
            Collection.missing collectionA

        doublesB =
            collectionB
                |> Collection.doubles
                |> List.map Tuple.first

        missingB =
            Collection.missing collectionB
    in
        ( intersect missingA doublesB, intersect missingB doublesA )


intersect : List Card.Card -> List Card.Card -> List Card.Card
intersect a b =
    let
        containedInA =
            (flip List.member) a
    in
        List.filter containedInA b

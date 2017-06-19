module TradeCard.Client exposing (main)

import Html
import TradeCard.Collection as Collection
import TradeCard.View as View


main : Program Never Model Message
main =
    Html.program
        {
          init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : (Model, Cmd msg)
init =
    let
        collection : Collection.Collection
        collection =
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
    in
        ({ collection = collection }, Cmd.none)


type alias Model =
    {
        collection: Collection.Collection
    }


type Message =
    DoNothing


update : Message -> Model -> (Model, Cmd Message)
update _ model =
    (model, Cmd.none)


view : Model -> Html.Html Message
view model =
    Html.div
        []
        (List.concat
             [
              [ View.collectionView model.collection ]
             ])


subscriptions : Model -> Sub Message
subscriptions _ = Sub.none

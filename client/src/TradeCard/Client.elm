module TradeCard.Client exposing (main)

import Html
import Html.Attributes as Attribute
import Html.Events as Event
import TradeCard.Card as Card
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
    ({ cardId = Nothing, collection = Collection.empty 1 15 } , Cmd.none)


type alias Model =
    {
      cardId: Maybe Int
    , collection: Collection.Collection
    }


type Message =
      DoNothing
    | CardId String
    | Collect


update : Message -> Model -> (Model, Cmd Message)
update message model =
    case message of
        DoNothing ->
            (model, Cmd.none)

        CardId representation ->
            let
                id = String.toInt representation
            in
                case id of
                    Ok cardId ->
                        ({ model | cardId = Just cardId }, Cmd.none)

                    Err _ ->
                        (model, Cmd.none)

        Collect ->
            case model.cardId of
                Just id  ->

                    let
                        card : Card.Card
                        card = { id = id }
                    in
                        case Collection.collect card model.collection of
                            Ok nextCollection ->
                                ({ model | collection = nextCollection, cardId = Nothing }, Cmd.none)

                            Err _ ->
                                (model, Cmd.none)

                Nothing ->
                    (model, Cmd.none)


view : Model -> Html.Html Message
view model =
    Html.div
        []
        (List.concat
             [
              [
                Html.div
                    []
                    [
                      Html.input [ Attribute.type_ "input", Event.onInput CardId ] []
                    , Html.button [ Event.onClick Collect ] [ Html.text "collect" ]
                    ]
              ]
             ,[ View.collectionView model.collection ]
             ])


subscriptions : Model -> Sub Message
subscriptions _ = Sub.none

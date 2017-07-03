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
    | UpdateCardId String
    | AddToCollection
    | Collect Card.Card
    | Trade Card.Card
    | Remove Card.Card


update : Message -> Model -> (Model, Cmd Message)
update message model =
    case message of
        DoNothing ->
            (model, Cmd.none)

        UpdateCardId representation ->
            let
                id = String.toInt representation
            in
                case id of
                    Ok cardId ->
                        ({ model | cardId = Just cardId }, Cmd.none)

                    Err _ ->
                        (model, Cmd.none)

        AddToCollection ->
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

        Collect card ->
            case Collection.collect card model.collection of
                Ok nextCollection ->
                    ({ model | collection = nextCollection, cardId = Nothing }, Cmd.none)

                Err _ ->
                    (model, Cmd.none)

        Trade card ->
            let
                nextCollection =
                    Collection.remove card model.collection
            in
                ({ model | collection = nextCollection, cardId = Nothing }, Cmd.none)

        Remove card ->
            let
                nextCollection =
                    Collection.remove card model.collection
            in
                ({ model | collection = nextCollection, cardId = Nothing }, Cmd.none)

view : Model -> Html.Html Message
view model =
    let
        inputValue =
            model.cardId
            |> Maybe.map toString
            |> Maybe.withDefault ""

        trade =
            \c -> Trade c

        collect =
            \c -> Collect c

        lose =
            Just (\c -> Remove c)
    in
        Html.div
            []
            [
              Html.div
                  [ Attribute.class "collector"]
                  [
                    Html.input
                       [
                         Attribute.type_ "input"
                       , Attribute.value inputValue
                       , Event.onInput UpdateCardId
                       ] []
                  , Html.button [ Event.onClick AddToCollection ] [ Html.text "collect" ]
                  ]
            , View.collectionView collect lose trade collect model.collection
            ]


subscriptions : Model -> Sub Message
subscriptions _ = Sub.none

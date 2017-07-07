module TradeCard.Client exposing (main)

import Html
import Html.Attributes as Attribute
import Html.Events as Event
import TradeCard.Card as Card
import TradeCard.Collection as Collection
import TradeCard.View as View


import Pouchdb
import Json.Encode as Encode
import Task


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
    (emptyModel 1 15 , Cmd.none)


type alias Model =
    {
      message : Maybe String
    , localDb : Pouchdb.Pouchdb
    , cardId: Maybe Int
    , collection: Collection.Collection
    }


emptyModel : Int -> Int -> Model
emptyModel low high =
    let
        localDb = Pouchdb.db "card-events" Pouchdb.dbOptions
    in
        {
          message = Nothing
        , localDb = localDb
        , cardId = Nothing
        , collection = Collection.empty low high
        }

type Message =
      DoNothing
    | UpdateCardId String
    | AddToCollection
    | Collect Card.Card
    | Trade Card.Card
    | Remove Card.Card
    | Post (Result Pouchdb.Fail Pouchdb.Post)


encodeEvent : EventType -> Card.Card -> Encode.Value
encodeEvent eventType card =
    Encode.object
        [
          ("type", Encode.string (toString eventType))
        , ("cardId", Encode.int card.id)
        ]

type EventType =
      Collected
    | Traded
    | Lost


update : Message -> Model -> (Model, Cmd Message)
update message model =
    case message of
        DoNothing ->
            (model, Cmd.none)

        Post msg ->
            let
                unpackedMessage =
                    unpack
                        (\m -> String.append "could not put message: " m.message)
                        (\m -> String.append "saved message with revision: " m.rev)
                        msg
            in
                ({ model | message = Just unpackedMessage }, Cmd.none)


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

                        task = (Pouchdb.post model.localDb (encodeEvent Collected card))

                        command = Task.attempt Post task
                    in
                        case Collection.collect card model.collection of
                            Ok nextCollection ->
                                ({ model | collection = nextCollection, cardId = Nothing }, command)

                            Err _ ->
                                (model, Cmd.none)

                Nothing ->
                    (model, Cmd.none)

        Collect card ->
            let
                task = (Pouchdb.post model.localDb (encodeEvent Collected card))

                command = Task.attempt Post task
            in
                case Collection.collect card model.collection of
                    Ok nextCollection ->
                        ({ model | collection = nextCollection, cardId = Nothing }, command)

                    Err _ ->
                        (model, Cmd.none)

        Trade card ->
            let
                task = (Pouchdb.post model.localDb (encodeEvent Traded card))

                command = Task.attempt Post task

                nextCollection =
                    Collection.remove card model.collection
            in
                ({ model | collection = nextCollection, cardId = Nothing }, command)

        Remove card ->
            let
                task = (Pouchdb.post model.localDb (encodeEvent Lost card))

                command = Task.attempt Post task

                nextCollection =
                    Collection.remove card model.collection
            in
                ({ model | collection = nextCollection, cardId = Nothing }, command)


unpack : (e -> b) -> (a -> b) -> Result e a -> b
unpack errFunc okFunc result =
    case result of
        Ok ok ->
            okFunc ok
        Err err ->
            errFunc err


view : Model -> Html.Html Message
view model =
    let
        inputValue =
            model.cardId
            |> Maybe.map toString
            |> Maybe.withDefault ""

        message =
            model.message
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
              Html.div [] [ Html.span [] [ Html.text message ] ]
            , Html.div
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

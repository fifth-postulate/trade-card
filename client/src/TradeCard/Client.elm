module TradeCard.Client exposing (main)

import Html
import Html.Attributes as Attribute
import Html.Events as Event
import TradeCard.Card as Card
import TradeCard.Collection as Collection
import TradeCard.View as View
import TradeCard.User as User
import Pouchdb
import Json.Encode as Encode
import Json.Decode as Decode
import Task


main : Program Flags Model Message
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Message )
init flags =
    let
        localDb =
            Pouchdb.db "card-events" Pouchdb.dbOptions

        request =
            Pouchdb.allDocsRequest
                |> (Pouchdb.include_docs True)

        task =
            Pouchdb.allDocs localDb request

        command =
            Task.attempt History task
    in
        ( emptyModel localDb flags.lowestCard flags.highestCard flags.user, command )


type alias Flags =
    { lowestCard : Int
    , highestCard : Int
    , user : String
    }


type alias Model =
    { localDb : Pouchdb.Pouchdb
    , cardId : String
    , nextEventId : Int
    , user : User.User
    , changingUser : Bool
    , collection : Collection.Collection
    }


emptyModel : Pouchdb.Pouchdb -> Int -> Int -> String -> Model
emptyModel localDb low high user =
    { localDb = localDb
    , cardId = ""
    , nextEventId = 1
    , user = user
    , changingUser = False
    , collection = Collection.empty low high
    }


type Message
    = DoNothing
    | UpdateCardId String
    | StartEditUser
    | StopEditUser
    | ChangeUser User.User
    | Collect Card.Card
    | Trade Card.Card
    | Remove Card.Card
    | Post (Result Pouchdb.Fail Pouchdb.Post)
    | History (Result Pouchdb.Fail (Pouchdb.AllDocs Encode.Value))


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        DoNothing ->
            ( model, Cmd.none )

        Post msg ->
            let
                unpackedMessage =
                    unpack
                        (\m -> String.append "could not put message: " m.message)
                        (\m -> String.append "saved message with revision: " m.rev)
                        msg
            in
                ( model, Cmd.none )

        History msg ->
            let
                onError model msg =
                    ( model, Cmd.none )

                onSuccess model msg =
                    let
                        filterMapFun aDoc =
                            case aDoc.doc of
                                Just document ->
                                    Result.toMaybe (Decode.decodeValue eventDecoder document)

                                Nothing ->
                                    Nothing

                        events =
                            List.filterMap filterMapFun msg.docs

                        maxId : CardEvent -> Int -> Int
                        maxId event current =
                            let
                                id =
                                    case event of
                                        Collected user id _ ->
                                            id

                                        Traded user id _ ->
                                            id

                                        Lost user id _ ->
                                            id
                            in
                                max current id

                        maxEventId =
                            List.foldr maxId 0 events

                        nextEventId =
                            1 + maxEventId

                        eventFromUser event =
                            case event of
                                Collected user _ _ ->
                                    user == model.user

                                Traded user _ _ ->
                                    user == model.user

                                Lost user _ _ ->
                                    user == model.user

                        userEvents =
                            List.filter eventFromUser events

                        updatedCollection =
                            List.foldr applyEvent model.collection userEvents
                    in
                        ( { model
                            | nextEventId = nextEventId
                            , collection = updatedCollection
                          }
                        , Cmd.none
                        )
            in
                unpack (onError model) (onSuccess model) msg

        UpdateCardId representation ->
            ( { model | cardId = representation }, Cmd.none )

        StartEditUser ->
            ( { model | changingUser = True }, Cmd.none )

        StopEditUser ->
            let
                request =
                    Pouchdb.allDocsRequest
                        |> (Pouchdb.include_docs True)

                task =
                    Pouchdb.allDocs model.localDb request

                command =
                    Task.attempt History task
            in
                ( emptyModel model.localDb (Tuple.first model.collection.range) (Tuple.second model.collection.range) model.user, command )

        ChangeUser user ->
            ( { model | user = user }, Cmd.none )

        Collect card ->
            let
                task =
                    (Pouchdb.post model.localDb (encodeEvent (Collected model.user model.nextEventId card)))

                command =
                    Task.attempt Post task
            in
                case Collection.collect card model.collection of
                    Ok nextCollection ->
                        ( { model
                            | collection = nextCollection
                            , nextEventId = model.nextEventId + 1
                            , cardId = ""
                          }
                        , command
                        )

                    Err _ ->
                        ( model, Cmd.none )

        Trade card ->
            let
                task =
                    (Pouchdb.post model.localDb (encodeEvent (Traded model.user model.nextEventId card)))

                command =
                    Task.attempt Post task

                nextCollection =
                    Collection.remove card model.collection
            in
                ( { model
                    | collection = nextCollection
                    , nextEventId = model.nextEventId + 1
                    , cardId = ""
                  }
                , command
                )

        Remove card ->
            let
                task =
                    (Pouchdb.post model.localDb (encodeEvent (Lost model.user model.nextEventId card)))

                command =
                    Task.attempt Post task

                nextCollection =
                    Collection.remove card model.collection
            in
                ( { model
                    | collection = nextCollection
                    , nextEventId = model.nextEventId + 1
                    , cardId = ""
                  }
                , command
                )


encodeEvent : CardEvent -> Encode.Value
encodeEvent eventType =
    let
        ( user, eventId, eventTypeRepresentation, cardId ) =
            case eventType of
                Collected user id card ->
                    ( user, id, "collected", card.id )

                Traded user id card ->
                    ( user, id, "traded", card.id )

                Lost user id card ->
                    ( user, id, "lost", card.id )
    in
        Encode.object
            [ ( "_id", Encode.string (String.concat [ user, ":", (zeroPad 15 eventId) ]) )
            , ( "type", Encode.string eventTypeRepresentation )
            , ( "cardId", Encode.int cardId )
            ]


zeroPad : Int -> Int -> String
zeroPad padLength n =
    let
        representation =
            toString n

        padding =
            pad "0" (padLength - (String.length representation))
    in
        padding ++ representation


pad : String -> Int -> String
pad symbol n =
    if n <= 0 then
        ""
    else
        symbol ++ (pad symbol (n - (String.length symbol)))


type CardEvent
    = Collected User.User Int Card.Card
    | Traded User.User Int Card.Card
    | Lost User.User Int Card.Card


eventDecoder : Decode.Decoder CardEvent
eventDecoder =
    let
        cardEventMapper : String -> String -> Int -> CardEvent
        cardEventMapper idRepresentation eventType cardId =
            let
                ( user, eventIdRepresentation ) =
                    let
                        default =
                            ( "", "0" )
                    in
                        case String.split ":" idRepresentation of
                            [] ->
                                default

                            [ _ ] ->
                                default

                            user :: eventIdRepresentation :: _ ->
                                ( user, eventIdRepresentation )

                stripped =
                    stripZero eventIdRepresentation

                eventId =
                    case String.toInt stripped of
                        Ok id ->
                            id

                        Err _ ->
                            0

                -- TODO improve
                card =
                    { id = cardId }
            in
                case eventType of
                    "collected" ->
                        Collected user eventId card

                    "traded" ->
                        Traded user eventId card

                    "lost" ->
                        Lost user eventId card

                    _ ->
                        Lost "" 0 card

        -- TODO this should be improved
    in
        Decode.map3
            cardEventMapper
            (Decode.field "_id" Decode.string)
            (Decode.field "type" Decode.string)
            (Decode.field "cardId" Decode.int)


stripZero : String -> String
stripZero word =
    dropWhile (\c -> c == '0') word


dropWhile : (Char -> Bool) -> String -> String
dropWhile predicate word =
    case String.uncons word of
        Just ( head, tail ) ->
            if (predicate head) then
                dropWhile predicate tail
            else
                word

        Nothing ->
            word


applyEvent : CardEvent -> Collection.Collection -> Collection.Collection
applyEvent event collection =
    case event of
        Collected user id card ->
            case Collection.collect card collection of
                Ok nextCollection ->
                    nextCollection

                Err _ ->
                    collection

        Traded user id card ->
            Collection.remove card collection

        Lost user id card ->
            Collection.remove card collection


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
        trade =
            \c -> Trade c

        collect =
            \c -> Collect c

        lose =
            Just (\c -> Remove c)
    in
        Html.div
            []
            [ Html.div
                [ Attribute.class "collector" ]
                (List.concat
                    [ [ Html.input
                            [ Attribute.type_ "input"
                            , Attribute.value model.cardId
                            , Event.onInput UpdateCardId
                            ]
                            []
                      ]
                    , [ User.view model.changingUser StartEditUser ChangeUser StopEditUser model.user ]
                    ]
                )
            , View.collectionView model.cardId collect lose trade collect model.collection
            ]


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none

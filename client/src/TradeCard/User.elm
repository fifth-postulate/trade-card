module TradeCard.User exposing (User, view)

import Html
import Html.Attributes as Attribute
import Html.Events as Event


type alias User =
    String


view : Bool -> msg -> (String -> msg) -> msg -> String -> Html.Html msg
view edit startEditingMessage changeUserMessage stopEditingMessage user =
    let
        content =
            if edit then
                Html.input
                    [ Attribute.defaultValue user
                    , Event.onInput changeUserMessage
                    , Event.onBlur stopEditingMessage
                    ]
                    []
            else
                Html.span [ Event.onClick startEditingMessage ] [ Html.text user ]
    in
        Html.div
            [ Attribute.classList [ ( "user", True ) ]
            ]
            [ content
            ]

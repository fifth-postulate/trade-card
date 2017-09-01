module TradeCard.User exposing (User, view)


import Html
import Html.Attributes as Attribute
import Html.Events as Event

type alias User = String

view : Bool -> msg -> String -> Html.Html msg
view edit editMessage user =
    let
        content =
            if edit then
                Html.input [ Attribute.defaultValue user ] []
            else
                Html.span [ Event.onClick editMessage ] [ Html.text user ]
    in
        Html.div
        [
         Attribute.classList [ ("user", True)]
        ]
        [
         content
        ]

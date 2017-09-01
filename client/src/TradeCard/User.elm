module TradeCard.User exposing (User, view)


import Html
import Html.Attributes as Attribute

type alias User = String

view : Bool -> String -> Html.Html msg
view edit user =
    let
        content =
            if edit then
                Html.input [ Attribute.defaultValue user ] []
            else
                Html.span [] [ Html.text user ]
    in
        Html.div
        [
         Attribute.classList [ ("user", True)]
        ]
        [
         content
        ]

module TradeCard.User exposing (view)


import Html
import Html.Attributes as Attribute

view : String -> Html.Html msg
view user =
    Html.div
        [
         Attribute.classList [ ("user", True)]
        ]
        [
         Html.span [] [ Html.text user ]
        ]

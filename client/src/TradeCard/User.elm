module TradeCard.User exposing (User, view)


import Html
import Html.Attributes as Attribute


type alias User = String

view : String -> Html.Html msg
view user =
    Html.div
        [
         Attribute.classList [ ("user", True)]
        ]
        [
         Html.span [] [ Html.text user ]
        ]

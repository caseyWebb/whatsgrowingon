module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (attribute)
import Ui.Modal as Modal exposing (ModalView)


type alias View msg =
    { title : String
    , body : List (Html msg)
    , modal : Maybe (ModalView msg)
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , body = [ Html.Styled.text str ]
    , modal = Nothing
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , body = List.map (Html.Styled.map fn) view.body
    , modal = Maybe.map (Modal.mapView fn) view.modal
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body =
        [ (case view.modal of
            Just modal ->
                [ div [ attribute "inert" "true" ] view.body
                , Modal.toHtml modal
                ]

            Nothing ->
                view.body
          )
            |> Html.Styled.div []
            |> Html.Styled.toUnstyled
        ]
    }

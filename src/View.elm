module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Css
import Css.Media
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (attribute, css)
import Maybe.Extra as Maybe


type alias View msg =
    { title : String
    , body : List (Html msg)
    , modal : Maybe (Html msg)
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
    , modal = Maybe.map (Html.Styled.map fn) view.modal
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument { title, body, modal } =
    { title = title
    , body =
        [ (case modal of
            Just modal_ ->
                [ div [ attribute "inert" "true" ] body
                , modal_
                ]

            Nothing ->
                body
          )
            |> div
                [ css
                    [ Css.fontFamily Css.sansSerif
                    , Css.width (Css.pct 100)
                    , Css.minHeight (Css.vh 100)
                    , Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                        [ Css.backgroundColor (Css.hex "030303")
                        , Css.color (Css.hex "ccc")
                        ]
                    ]
                ]
            |> Html.Styled.toUnstyled
        ]
    }

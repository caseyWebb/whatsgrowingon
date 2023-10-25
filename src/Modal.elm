module Modal exposing (..)

import Css
import Html.Styled as Html exposing (Html, button, div, form, text)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events exposing (onClick)


type Modal
    = AddPlantingModal


view : { closeModal : msg } -> Modal -> Html msg
view config modal =
    Html.div []
        [ viewBackdrop config
        , viewModal config <|
            case modal of
                AddPlantingModal ->
                    div [] [ text "Add Planting" ]
        ]


viewBackdrop : { closeModal : msg } -> Html msg
viewBackdrop config =
    Html.div
        [ Attrs.css
            [ Css.position Css.fixed
            , Css.top (Css.px 0)
            , Css.left (Css.px 0)
            , Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            , Css.backgroundColor (Css.rgba 0 0 0 0.8)
            ]
        , onClick config.closeModal
        ]
        []


viewModal : { closeModal : msg } -> Html msg -> Html msg
viewModal config content =
    Html.div
        [ Attrs.css
            [ Css.position Css.absolute
            , Css.top (Css.px 50)
            , Css.left (Css.px 50)
            , Css.width (Css.pct 50)
            , Css.height (Css.pct 50)
            , Css.backgroundColor (Css.rgba 255 255 255 1)
            ]
        ]
        [ viewCloseButton config
        , content
        ]


viewCloseButton : { closeModal : msg } -> Html msg
viewCloseButton config =
    Html.button
        [ Attrs.css
            [ Css.position Css.absolute
            , Css.top (Css.px 0)
            , Css.right (Css.px 0)
            , Css.width (Css.px 50)
            , Css.height (Css.px 50)
            , Css.backgroundColor (Css.rgba 0 0 0 0.8)
            ]
        , onClick config.closeModal
        ]
        [ text "X" ]

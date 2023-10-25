module Modal exposing (..)

import Css
import Html.Styled as Html exposing (Html, button, div, form, h1, text)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events exposing (onClick)


type Modal modal
    = PageModal modal


view : { closeModal : msg } -> Modal modal -> Html msg
view config modal =
    Html.div
        [ Attrs.css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            , Css.position Css.fixed
            , Css.top Css.zero
            , Css.right Css.zero
            , Css.bottom Css.zero
            , Css.left Css.zero
            ]
        ]
        [ viewBackdrop config
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
            [ Css.backgroundColor (Css.hex "111")
            , Css.borderRadius (Css.px 6)
            , Css.color (Css.hex "fff")
            , Css.width (Css.px 600)
            , Css.maxWidth (Css.vw 90)
            , Css.position Css.relative
            , Css.padding2 Css.zero (Css.px 20)
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
            , Css.top (Css.px -30)
            , Css.right (Css.px -30)
            , Css.width (Css.px 30)
            , Css.height (Css.px 30)
            , Css.color (Css.hex "fff")
            , Css.backgroundColor (Css.rgba 0 0 0 1)
            , Css.borderRadius (Css.pct 50)
            , Css.border3 (Css.px 3) Css.solid (Css.hex "fff")
            , Css.outline Css.none
            , Css.fontWeight Css.bold
            , Css.fontSize (Css.px 20)
            ]
        , onClick config.closeModal
        ]
        [ text "X" ]


viewAddPlantingModal : Html msg
viewAddPlantingModal =
    Html.div
        []
        [ h1 [] [ text "Add Planting" ]
        , form []
            []
        ]

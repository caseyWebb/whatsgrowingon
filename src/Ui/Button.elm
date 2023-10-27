module Ui.Button exposing (..)

import Css
import Css.Media
import Html.Styled exposing (button)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)


view :
    msg
    -> List (Html.Styled.Attribute msg)
    -> List (Html.Styled.Html msg)
    -> Html.Styled.Html msg
view clickHandler attrs content =
    button
        (css
            [ Css.fontWeight Css.bold
            , Css.fontSize (Css.px 18)
            , Css.outline Css.none
            , Css.border Css.zero
            , Css.borderRadius (Css.px 4)
            , Css.padding2 (Css.em 0.5) (Css.em 1)
            , Css.backgroundColor (Css.rgba 0 0 0 0.02)
            , Css.color (Css.rgba 0 0 0 0.4)
            , Css.hover
                [ Css.color (Css.hex "000")
                , Css.backgroundColor (Css.rgba 0 0 0 0.03)
                ]
            , Css.focus
                [ Css.color (Css.hex "4f518c")
                , Css.outline3 (Css.px 5) Css.solid (Css.hex "4f518c")
                , Css.outlineOffset (Css.px 5)
                ]
            , Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                [ Css.backgroundColor (Css.rgba 255 255 255 0.1)
                , Css.color (Css.rgba 255 255 255 0.8)
                , Css.hover
                    [ Css.color (Css.hex "fff")
                    , Css.backgroundColor (Css.rgba 255 255 255 0.2)
                    ]
                , Css.focus
                    [ Css.color (Css.hex "fff")
                    , Css.outlineColor (Css.hex "fff")
                    ]
                ]
            ]
            :: onClick clickHandler
            :: attrs
        )
        content

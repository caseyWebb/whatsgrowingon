module Ui.FocusRing exposing (..)

import Css exposing (..)
import Css.Media exposing (withMediaQuery)


focusRing : Style
focusRing =
    batch
        [ focus
            [ outline3 (px 5) solid (hex "4f518c")
            , outlineOffset (px 5)
            , borderRadius (px 5)
            ]
        , withMediaQuery [ "(prefers-color-scheme: dark)" ]
            [ focus
                [ outlineColor (hex "fff")
                ]
            ]
        ]

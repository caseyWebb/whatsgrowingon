module Ui.Color exposing (Color(..), styles)

import Css


type Color
    = Red
    | Pink
    | Orange
    | Yellow
    | Green
    | DarkGreen
    | Purple
    | White


styles : Color -> Css.Style
styles color =
    Css.important <|
        Css.batch
            [ Css.backgroundColor (toCss color)
            , Css.color (foregroundFor color)
            ]


toCss : Color -> Css.Color
toCss color =
    case color of
        Red ->
            -- Css.hex "f72634"
            Css.hex "d1105a"

        Pink ->
            Css.hex "ef7a85"

        Orange ->
            Css.hex "ff9d47"

        Yellow ->
            Css.hex "ffeb99"

        Green ->
            -- Css.hex "224c25"
            Css.hex "b5ca8d"

        DarkGreen ->
            Css.hex "8bb174"

        Purple ->
            -- Css.hex "3e0b5e"
            Css.hex "a786db"

        White ->
            Css.hex "f9f5e3"


foregroundFor : Color -> Css.Color
foregroundFor color =
    let
        light =
            Css.rgba 255 255 255 0.8

        dark =
            Css.rgba 0 0 0 0.8
    in
    case color of
        White ->
            dark

        Red ->
            light

        Pink ->
            light

        Orange ->
            light

        Yellow ->
            dark

        Green ->
            light

        DarkGreen ->
            light

        Purple ->
            light

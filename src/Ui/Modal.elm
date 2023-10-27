module Ui.Modal exposing (Modal, Model, Msg, closed, map, modal, open, subscriptions, toHtml, update, view)

import Browser.Events exposing (onKeyDown)
import Css
import Css.Media
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode


type Model modalKind
    = Open modalKind
    | Close


type Msg
    = CloseModal
    | NoOp


type Modal msg
    = Modal (Html msg)


map : (a -> b) -> Modal a -> Modal b
map f (Modal html) =
    Modal (Html.map f html)


modal : Html msg -> Modal msg
modal =
    Modal


closed : Model modalKind
closed =
    Close


open : modalKind -> Model modalKind
open =
    Open


update : Msg -> Model modalKind -> Model modalKind
update msg model =
    case msg of
        CloseModal ->
            Close

        NoOp ->
            model


subscriptions : Sub Msg
subscriptions =
    onKeyDown (Decode.map toKey (Decode.field "key" Decode.string))


toKey : String -> Msg
toKey key =
    case key of
        "Escape" ->
            CloseModal

        _ ->
            NoOp


view : (Msg -> msg) -> (modalKind -> Html msg) -> Model modalKind -> Maybe (Modal msg)
view toMsg modalKindContents model =
    (case model of
        Open modalKind ->
            Just (modalKindContents modalKind |> viewModal toMsg)

        Close ->
            Nothing
    )
        |> Maybe.map Modal


viewModal : (Msg -> msg) -> Html msg -> Html msg
viewModal toMsg contents =
    div
        [ css
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
        [ viewModalBackdrop toMsg
        , viewModalContent toMsg contents
        ]


viewModalBackdrop : (Msg -> msg) -> Html msg
viewModalBackdrop toMsg =
    div
        [ css
            [ Css.position Css.fixed
            , Css.top (Css.px 0)
            , Css.left (Css.px 0)
            , Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            , Css.backgroundColor (Css.rgba 255 255 255 0.8)
            , Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                [ Css.backgroundColor (Css.rgba 0 0 0 0.95)
                ]
            ]
        , onClick (toMsg CloseModal)
        ]
        []


viewModelCloseButton : (Msg -> msg) -> Html msg
viewModelCloseButton toMsg =
    Html.button
        [ css
            [ Css.position Css.absolute
            , Css.top (Css.px -30)
            , Css.right (Css.px -30)
            , Css.width (Css.px 30)
            , Css.height (Css.px 30)
            , Css.fontWeight Css.bold
            , Css.fontSize (Css.px 30)
            , Css.border Css.zero
            , Css.outline Css.none
            , Css.backgroundColor (Css.hex "fff")
            , Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                [ Css.color (Css.hex "fff")
                , Css.backgroundColor (Css.hex "000")
                ]
            ]
        , onClick (toMsg CloseModal)
        ]
        [ text "X" ]


viewModalContent : (Msg -> msg) -> Html msg -> Html msg
viewModalContent toMsg content =
    Html.div
        [ css
            [ Css.borderRadius (Css.px 6)
            , Css.width (Css.px 1200)
            , Css.maxWidth (Css.vw 90)
            , Css.position Css.relative
            ]
        ]
        [ viewModelCloseButton toMsg
        , Html.div
            [ css
                [ Css.overflowX Css.hidden
                , Css.overflowY Css.auto
                , Css.maxHeight (Css.vh 100)
                ]
            ]
            [ content ]
        ]


toHtml : Modal msg -> Html msg
toHtml (Modal html) =
    html

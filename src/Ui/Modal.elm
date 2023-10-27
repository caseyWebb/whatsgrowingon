module Ui.Modal exposing (ModalView, Model, Msg, closed, kind, mapModel, mapView, modal, open, subscriptions, toHtml, update, view)

import Browser.Events exposing (onKeyDown)
import Css
import Css.Media
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode
import Task


type Model modalKind msg
    = Opened modalKind msg
    | Closed


mapModel : (a -> b) -> Model a msg -> Model b msg
mapModel f model =
    case model of
        Opened modalKind msg ->
            Opened (f modalKind) msg

        Closed ->
            Closed


type Msg
    = Close
    | NoOp


type ModalView msg
    = ModalView (Html msg)


mapView : (a -> b) -> ModalView a -> ModalView b
mapView f (ModalView html) =
    ModalView (Html.map f html)


modal : Html msg -> ModalView msg
modal =
    ModalView


kind : Model modalKind msg -> Maybe modalKind
kind model =
    case model of
        Opened modalKind _ ->
            Just modalKind

        Closed ->
            Nothing


closed : Model modalKind msg
closed =
    Closed


open : modalKind -> msg -> Model modalKind msg
open =
    Opened


update : Msg -> Model modalKind msg -> ( Model modalKind msg, Cmd msg )
update msg model =
    case ( msg, model ) of
        ( Close, Opened _ onClose ) ->
            ( Closed
            , Task.succeed onClose |> Task.perform identity
            )

        ( Close, Closed ) ->
            ( Closed, Cmd.none )

        ( NoOp, _ ) ->
            ( model, Cmd.none )


subscriptions : Sub Msg
subscriptions =
    onKeyDown
        (Decode.map
            (\key ->
                case key of
                    "Escape" ->
                        Close

                    _ ->
                        NoOp
            )
            (Decode.field "key" Decode.string)
        )


view : (Msg -> msg) -> (modalKind -> Html msg) -> Model modalKind msg -> Maybe (ModalView msg)
view toMsg modalKindContents model =
    (case model of
        Opened modalKind _ ->
            Just (modalKindContents modalKind |> viewModal toMsg)

        Closed ->
            Nothing
    )
        |> Maybe.map ModalView


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
        , onClick (toMsg Close)
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
        , onClick (toMsg Close)
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


toHtml : ModalView msg -> Html msg
toHtml (ModalView html) =
    html

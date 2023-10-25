module Pages.Home_ exposing (Model, Msg, page)

import Browser.Events exposing (onKeyDown, onKeyUp)
import Css
import Css.Media as Media
import Data exposing (..)
import Effect exposing (Effect)
import GenericDict as Dict
import Html.Styled as Html
import Html.Styled.Attributes as Attrs
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Json.Decode as Decode
import Modal
import Page
import RemoteData exposing (RemoteData(..))
import Request exposing (Request)
import Shared
import Slug exposing (Slug)
import View exposing (View)


type alias Model =
    ()


type Msg
    = AddZone
    | UpdateZone Bool Zone
    | ShowNewPlantingModal


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Effect Msg )
init =
    ( (), Effect.none )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        AddZone ->
            ( model, Effect.fromShared Shared.addZone )

        UpdateZone save zone ->
            ( model, Effect.fromShared (Shared.updateZone save zone) )

        ShowNewPlantingModal ->
            ( model, Effect.none )



-- ( model, Effect.fromShared (Shared.showModal Modal.AddPlantingModal) )


view : Shared.Model -> Model -> View Msg
view shared _ =
    { title = "What's growing on!?"
    , body =
        case shared.zones of
            NotAsked ->
                [ Html.p [] [ Html.text "Loading..." ] ]

            Loading ->
                [ Html.p [] [ Html.text "Loading..." ] ]

            Failure err ->
                [ Html.p [] [ Html.text <| "Error: " ++ err ] ]

            Success zones ->
                [ Html.ul
                    [ Attrs.css
                        [ Css.displayFlex
                        , Css.flexDirection Css.column
                        , Css.listStyle Css.none
                        , Css.width (Css.pct 100)
                        , Css.maxWidth (Css.px 600)
                        , Css.property "gap" "1em"
                        ]
                    ]
                    (List.map
                        (\zone ->
                            Html.li
                                [ Attrs.css
                                    [ Css.displayFlex
                                    , Css.flexDirection Css.column
                                    , Css.property "gap" "1em"
                                    , Css.padding2 (Css.em 1) (Css.em 1)
                                    ]
                                ]
                                [ Html.input
                                    [ Attrs.value zone.name
                                    , Attrs.css
                                        [ Css.backgroundColor Css.inherit
                                        , Css.color Css.inherit
                                        , Css.fontWeight Css.bold
                                        , Css.fontSize (Css.px 14)
                                        , Css.border Css.zero
                                        , Css.focus [ Css.outline Css.none ]
                                        ]
                                    , onInput (\name -> UpdateZone False { zone | name = name })
                                    , onBlur (UpdateZone True zone)
                                    ]
                                    []
                                , Html.div
                                    [ Attrs.css
                                        [ Css.displayFlex
                                        , Css.justifyContent Css.spaceBetween
                                        , Css.property "gap" "10px"
                                        ]
                                    ]
                                    [ Html.div
                                        [ Attrs.css
                                            [ Css.color (Css.hex "fff")
                                            , Css.backgroundColor (Css.hex "4c1010")
                                            , Css.padding (Css.px 20)
                                            , Css.borderRadius (Css.px 4)
                                            ]
                                        ]
                                        [ Html.div
                                            [ Attrs.css
                                                [ Css.displayFlex
                                                , Css.property "gap" "10px"
                                                , Css.alignItems Css.center
                                                ]
                                            ]
                                            [ Html.span
                                                [ Attrs.css
                                                    [ Css.textTransform Css.uppercase
                                                    , Css.fontWeight Css.bold
                                                    , Css.fontSize (Css.px 18)
                                                    ]
                                                ]
                                                [ Html.text "Beets" ]
                                            , Html.span
                                                [ Attrs.css
                                                    [ Css.fontSize (Css.px 12)
                                                    , Css.displayFlex
                                                    , Css.flexDirection Css.column
                                                    ]
                                                ]
                                                [ Html.span [] [ Html.text "Formanova" ]
                                                , Html.span [] [ Html.text "2021-04-1" ]
                                                ]
                                            ]
                                        ]
                                    , Html.button
                                        [ onClick ShowNewPlantingModal
                                        , Attrs.css
                                            [ Css.fontWeight Css.bold
                                            , Css.fontSize (Css.px 18)
                                            , Css.backgroundColor Css.inherit
                                            , Css.flexGrow (Css.int 1)
                                            , Css.border3 (Css.px 1) Css.solid (Css.hex "333")
                                            , Css.padding2 (Css.em 0.5) (Css.em 1)
                                            , Css.outline Css.none
                                            , Css.borderRadius (Css.px 4)
                                            , Css.color (Css.hex "000")
                                            , Css.hover
                                                [ Css.borderColor (Css.hex "000")
                                                , Css.color (Css.hex "fff")
                                                ]
                                            , Css.focus
                                                [ Css.borderColor (Css.hex "000")
                                                , Css.color (Css.hex "fff")
                                                , Css.outline3 (Css.px 5) Css.solid (Css.hex "000")
                                                , Css.outlineOffset (Css.px 5)
                                                ]
                                            , Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                                                [ Css.color (Css.hex "fff")
                                                , Css.hover
                                                    [ Css.borderColor (Css.hex "fff")
                                                    , Css.color (Css.hex "fff")
                                                    ]
                                                , Css.focus
                                                    [ Css.borderColor (Css.hex "fff")
                                                    , Css.color (Css.hex "fff")
                                                    , Css.outlineColor (Css.hex "fff")
                                                    ]
                                                ]
                                            ]
                                        ]
                                        [ Html.text "+" ]
                                    ]
                                ]
                        )
                        (Dict.values zones)
                    )
                , Html.button
                    [ onClick AddZone
                    , Attrs.css
                        [ Css.fontWeight Css.bold
                        , Css.fontSize (Css.px 18)
                        , Css.backgroundColor Css.inherit
                        , Css.border3 (Css.px 1) Css.solid (Css.hex "333")
                        , Css.padding2 (Css.em 0.5) (Css.em 1)
                        , Css.outline Css.none
                        , Css.borderRadius (Css.px 4)
                        , Css.color (Css.hex "000")
                        , Css.hover
                            [ Css.borderColor (Css.hex "000")
                            , Css.color (Css.hex "fff")
                            ]
                        , Css.focus
                            [ Css.borderColor (Css.hex "000")
                            , Css.color (Css.hex "fff")
                            , Css.outline3 (Css.px 5) Css.solid (Css.hex "000")
                            , Css.outlineOffset (Css.px 5)
                            ]
                        , Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                            [ Css.color (Css.hex "fff")
                            , Css.hover
                                [ Css.borderColor (Css.hex "fff")
                                , Css.color (Css.hex "fff")
                                ]
                            , Css.focus
                                [ Css.borderColor (Css.hex "fff")
                                , Css.color (Css.hex "fff")
                                , Css.outlineColor (Css.hex "fff")
                                ]
                            ]
                        ]
                    ]
                    [ Html.text "Add a new zone" ]
                ]
    , modal = Nothing
    }

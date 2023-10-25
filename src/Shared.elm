module Shared exposing
    ( Modal(..)
    , Model
    , Msg
    , ToBackend(..)
    , ToFrontend(..)
    , addZone
    , fromBackend
    , init
    , showModal
    , subscriptions
    , update
    , updateZone
    , view
    )

import Css
import Css.Media
import Data exposing (..)
import GenericDict as Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (attribute, css)
import Html.Styled.Events exposing (onClick)
import Random
import RemoteData exposing (RemoteData)
import Request exposing (Request)
import Slug exposing (Slug)
import View exposing (View)


addZone : Msg
addZone =
    AddZone


updateZone : Bool -> Zone -> Msg
updateZone =
    UpdateZone


fromBackend : ToFrontend -> Msg
fromBackend =
    FromBackend


showModal : Modal -> Msg
showModal =
    ShowModal



-- INIT


type alias Model =
    { zones : RemoteData String (Dict Slug Zone)
    , modal : Maybe Modal
    }


type Modal
    = AddPlantingModal


init : { toBackend : ToBackend -> Cmd msg } -> Request -> ( Model, Cmd msg )
init { toBackend } _ =
    ( { zones = RemoteData.Loading
      , modal = Nothing
      }
    , toBackend <| FetchZones
    )



-- UPDATE


type Msg
    = FromBackend ToFrontend
    | AddZone
    | UpdateZone Bool Zone
    | GotNewSlug Slug
    | ShowModal Modal
    | CloseModal


type ToBackend
    = FetchZones
    | SaveZone Zone


type ToFrontend
    = GotZones (Dict Slug Zone)


update : { toBackend : ToBackend -> Cmd Msg } -> Request -> Msg -> Model -> ( Model, Cmd Msg )
update { toBackend } _ msg model =
    case msg of
        FromBackend (GotZones zones) ->
            ( { model | zones = RemoteData.Success zones }
            , Cmd.none
            )

        AddZone ->
            ( model
            , Random.generate GotNewSlug Slug.random
            )

        GotNewSlug slug ->
            let
                zone =
                    Slug.map
                        (\str ->
                            { slug = slug
                            , name = str
                            , plantings = []
                            }
                        )
                        slug
            in
            ( { model
                | zones =
                    RemoteData.map (Dict.insert (Slug.map identity) slug zone) model.zones
              }
            , toBackend (SaveZone zone)
            )

        UpdateZone save zone ->
            ( { model
                | zones =
                    RemoteData.map (Dict.insert (Slug.map identity) zone.slug zone) model.zones
              }
            , if save then
                toBackend (SaveZone zone)

              else
                Cmd.none
            )

        ShowModal modal ->
            ( { model | modal = Just modal }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modal = Nothing }
            , Cmd.none
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none



-- VIEW


view :
    Request
    -> { page : View msg, toMsg : Msg -> msg }
    -> Model
    -> View msg
view _ { page, toMsg } model =
    { title = page.title
    , body =
        [ (case model.modal of
            Just modal_ ->
                [ div [ attribute "inert" "true" ] page.body
                , viewModal model modal_ |> Html.map toMsg
                ]

            Nothing ->
                page.body
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
        ]
    }


viewModal : Model -> Modal -> Html Msg
viewModal model modal =
    Html.div
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
        [ viewModalBackdrop
        , viewModalContent <|
            case modal of
                AddPlantingModal ->
                    viewAddPlantingModal
        ]


viewModalBackdrop : Html Msg
viewModalBackdrop =
    div
        [ css
            [ Css.position Css.fixed
            , Css.top (Css.px 0)
            , Css.left (Css.px 0)
            , Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            , Css.backgroundColor (Css.rgba 0 0 0 0.8)
            ]
        , onClick CloseModal
        ]
        []


viewModelCloseButton : Html Msg
viewModelCloseButton =
    Html.button
        [ css
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
        , onClick CloseModal
        ]
        [ text "X" ]


viewModalContent : Html Msg -> Html Msg
viewModalContent content =
    Html.div
        [ css
            [ Css.backgroundColor (Css.hex "111")
            , Css.borderRadius (Css.px 6)
            , Css.color (Css.hex "fff")
            , Css.width (Css.px 600)
            , Css.maxWidth (Css.vw 90)
            , Css.position Css.relative
            , Css.padding2 Css.zero (Css.px 20)
            ]
        ]
        [ viewModelCloseButton
        , content
        ]


viewAddPlantingModal : Html Msg
viewAddPlantingModal =
    Html.div
        []
        [ Html.h1 [] [ text "Add Planting" ]
        , Html.p [] [ text "TODO" ]
        ]

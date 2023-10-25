module Shared exposing
    ( Model
    , Msg
    , ToBackend(..)
    , ToFrontend(..)
    , addZone
    , fromBackend
    , init
    , subscriptions
    , update
    , updateZone
    , view
    )

import Css
import Css.Global
import Css.Media
import Data exposing (..)
import GenericDict as Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attrs
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



-- INIT


type alias Model =
    { zones : RemoteData String (Dict Slug Zone)
    }


init : { toBackend : ToBackend -> Cmd msg } -> Request -> ( Model, Cmd msg )
init { toBackend } _ =
    ( { zones = RemoteData.Loading }
    , toBackend <| FetchZones
    )



-- UPDATE


type Msg
    = FromBackend ToFrontend
    | AddZone
    | UpdateZone Bool Zone
    | GotNewSlug Slug
    | Noop


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

        Noop ->
            ( model
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
view req { page, toMsg } model =
    let
        theme =
            Css.Global.body []
    in
    { title =
        page.title
    , body =
        [ Css.Global.global
            [ Css.Global.body
                [ Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                    [ Css.backgroundColor (Css.hex "111")
                    , Css.color (Css.hex "ccc")
                    ]
                ]
            ]
        , Html.div
            [ Attrs.css
                [ Css.width (Css.pct 100)
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.alignItems Css.center
                , Css.fontFamily Css.sansSerif
                , Css.textTransform Css.lowercase
                ]
            ]
            page.body
        ]
    }

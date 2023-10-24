module Shared exposing
    ( Model
    , Msg
    , ToBackend(..)
    , ToFrontend(..)
    , fetchZones
    , fromBackend
    , init
    , subscriptions
    , update
    , view
    )

import Data exposing (..)
import GenericDict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class)
import RemoteData exposing (RemoteData)
import Request exposing (Request)
import Slug exposing (Slug)
import View exposing (View)


fetchZones : Msg
fetchZones =
    ToBackend FetchZones


fromBackend : ToFrontend -> Msg
fromBackend =
    FromBackend



-- INIT


type alias Model =
    { zones : RemoteData String (Dict Slug Zone)
    }


init : { toBackend : ToBackend -> Cmd msg } -> Request -> ( Model, Cmd msg )
init _ _ =
    ( { zones = RemoteData.NotAsked }
    , Cmd.none
    )



-- UPDATE


type Msg
    = FromBackend ToFrontend
    | ToBackend ToBackend
    | Noop


type ToBackend
    = FetchZones


type ToFrontend
    = GotZones (Dict Slug Zone)


update : { toBackend : ToBackend -> Cmd msg } -> Request -> Msg -> Model -> ( Model, Cmd msg )
update { toBackend } _ msg model =
    case msg of
        FromBackend (GotZones zones) ->
            ( { model | zones = RemoteData.Success zones }
            , Cmd.none
            )

        ToBackend (FetchZones as toBackendMsg) ->
            ( { model | zones = RemoteData.Loading }
            , toBackend toBackendMsg
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
    { title =
        page.title
    , body =
        [ div [ class "layout" ]
            [ div [ class "page" ] page.body
            ]
        ]
    }

module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , ToBackend(..)
    , ToFrontend(..)
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



-- INIT


type alias Flags =
    ()


type alias Model =
    { zones : RemoteData String (Dict Slug Zone)
    }


init : { toBackend : ToBackend -> Cmd msg } -> Request -> Flags -> ( Model, Cmd msg )
init { toBackend } _ _ =
    ( { zones = RemoteData.Loading }
    , toBackend FetchZones
    )



-- UPDATE


type Msg
    = FromBackend ToFrontend
    | Noop


type ToBackend
    = FetchZones


type ToFrontend
    = GotZones (Dict Slug Zone)


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        FromBackend (GotZones zones) ->
            ( { model
                | zones = RemoteData.Success zones
              }
            , Cmd.none
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

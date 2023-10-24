module Pages.Home_ exposing (Model, Msg, page)

import Data exposing (..)
import Effect exposing (Effect)
import Gen.Route as Route
import Html exposing (Attribute)
import Html.Events exposing (onClick)
import Page exposing (Page)
import RemoteData exposing (RemoteData)
import Request exposing (Request)
import Shared
import Slug
import View exposing (View)


type alias Model =
    ()


type Msg
    = AddZone


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Effect Msg )
init =
    ( (), Effect.fromShared <| Shared.fetchZones )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        AddZone ->
            ( model, Effect.none )


view : Model -> View msg
view _ =
    { title = "What's growing on!?"
    , body =
        [ Html.h1 [] [ Html.text "What's growing on!?" ]
        , Html.p [] [ Html.text "A simple app to help you keep track of what's growing in your garden." ]

        -- , Html.button [ onClick AddZone ] [ Html.text "Add a new zone" ]
        ]
    }
module Backend exposing (..)

import GenericDict as Dict
import Lamdera exposing (ClientId, SessionId)
import Shared
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { zones = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        respond msg_ =
            ( model, Lamdera.sendToFrontend clientId msg_ )
    in
    case msg of
        SharedToBackend toBackendMsg ->
            case toBackendMsg of
                Shared.FetchZones ->
                    respond <| SharedToFrontend (Shared.GotZones model.zones)

        NoOpToBackend ->
            ( model, Cmd.none )

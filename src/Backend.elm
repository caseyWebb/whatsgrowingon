module Backend exposing (..)

import GenericDict as Dict
import Lamdera exposing (ClientId, SessionId)
import Shared
import Slug
import Types exposing (..)


type alias Model =
    BackendModel


type alias Msg =
    BackendMsg


app :
    { init : ( Model, Cmd Msg )
    , update : Msg -> Model -> ( Model, Cmd Msg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd Msg )
    , subscriptions : Model -> Sub Msg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { zones = Dict.empty
      , crops = Dict.empty
      , varieties = Dict.empty
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd Msg )
updateFromFrontend _ clientId msg model =
    let
        respond msg_ =
            ( model, Lamdera.sendToFrontend clientId msg_ )
    in
    case msg of
        SharedToBackend toBackendMsg ->
            case toBackendMsg of
                Shared.FetchZones ->
                    respond <| SharedToFrontend (Shared.GotZones model.zones)

                Shared.SaveZone zone ->
                    ( { model | zones = Dict.insert (Slug.map identity) zone.slug zone model.zones }
                    , Cmd.none
                    )

        NoOpToBackend ->
            ( model, Cmd.none )

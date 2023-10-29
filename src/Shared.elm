module Shared exposing
    ( Model
    , Msg
    , ToBackend(..)
    , ToFrontend(..)
    , addZone
    , deleteZone
    , fromBackend
    , init
    , subscriptions
    , update
    , updateZone
    , view
    )

import Browser.Dom as Dom
import Data exposing (..)
import Data.PasskeyRegistrationOptions exposing (PasskeyRegistrationOptions)
import Data.PasskeyRegistrationResponse exposing (PasskeyRegistrationResponse)
import Data.Users as Users exposing (User)
import GenericDict as Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Random
import RemoteData exposing (RemoteData)
import Request exposing (Request)
import Slug exposing (Slug)
import Task
import Time
import Ui.Button as Button
import View exposing (View)
import WebAuthn exposing (createCredential, createCredentialResponse)


addZone : Msg
addZone =
    AddZone Nothing


updateZone : Bool -> Zone -> Msg
updateZone =
    UpdateZone


fromBackend : ToFrontend -> Msg
fromBackend =
    FromBackend


deleteZone : Slug -> Msg
deleteZone =
    DeleteZone



-- INIT


type alias Model =
    { data :
        RemoteData
            String
            { zones : Dict Slug Zone
            , crops : Dict Slug Crop
            , varieties : Dict Slug Variety
            , currentUser : Maybe User
            }
    , now : Maybe Time.Posix
    }


init : { toBackend : ToBackend -> Cmd Msg } -> Request -> ( Model, Cmd Msg )
init { toBackend } _ =
    ( { data = RemoteData.Loading
      , now = Nothing
      }
    , Cmd.batch
        [ toBackend <| FetchData
        , Time.now |> Task.perform GotCurrentTime
        ]
    )



-- UPDATE


type Msg
    = FromBackend ToFrontend
    | AddZone (Maybe Slug)
    | DeleteZone Slug
    | FocusZone Slug
    | UpdateZone Bool Zone
    | GotCurrentTime Time.Posix
    | StartSignupFlow
    | GotPasskeyRegistrationResponse (Result String PasskeyRegistrationResponse)
    | NoOp


type ToBackend
    = FetchData
    | SaveZone Zone
    | DeleteZoneToBackend Slug
    | FetchPasskeyRegistrationOptions
    | VerifyPasskeyRegistrationResponse PasskeyRegistrationResponse


type ToFrontend
    = GotData
        { zones : Dict Slug Zone
        , crops : Dict Slug Crop
        , varieties : Dict Slug Variety
        , currentUser : Maybe User
        }
    | GotWebAuthnRegistrationOptions (Result String PasskeyRegistrationOptions)


update :
    { toBackend : ToBackend -> Cmd Msg
    }
    -> Request
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update ({ toBackend } as config) req msg model =
    let
        andThen msg_ ( updatedModel, cmd ) =
            update config req msg_ updatedModel |> Tuple.mapSecond (List.singleton >> (::) cmd >> Cmd.batch)
    in
    case msg of
        FromBackend (GotData data) ->
            ( { model
                | data = RemoteData.Success data
              }
            , Cmd.none
            )

        FromBackend (GotWebAuthnRegistrationOptions challenge) ->
            ( model
            , case challenge of
                Ok challenge_ ->
                    createCredential challenge_

                Err err ->
                    Debug.log "Failed to create credential" err
                        |> always Cmd.none
            )

        AddZone maybeSlug ->
            case maybeSlug of
                Nothing ->
                    ( model
                    , Random.generate (AddZone << Just) Slug.random
                    )

                Just slug ->
                    let
                        index =
                            RemoteData.map (.zones >> Dict.size) model.data
                                |> RemoteData.toMaybe
                                |> Maybe.withDefault 0

                        newZone =
                            Zone slug index "" []
                    in
                    update config req (UpdateZone True newZone) model
                        |> andThen (FocusZone slug)

        FocusZone slug ->
            ( model, Slug.map Dom.focus slug |> Task.attempt (always NoOp) )

        UpdateZone save zone ->
            ( { model
                | data =
                    RemoteData.map
                        (\data ->
                            { data
                                | zones = Dict.insert (Slug.map identity) zone.slug zone data.zones
                            }
                        )
                        model.data
              }
            , if save then
                toBackend (SaveZone zone)

              else
                Cmd.none
            )

        DeleteZone zone ->
            ( { model
                | data =
                    RemoteData.map
                        (\data ->
                            { data
                                | zones = Dict.remove (Slug.map identity) zone data.zones
                            }
                        )
                        model.data
              }
            , toBackend (DeleteZoneToBackend zone)
            )

        GotCurrentTime maybePosix ->
            ( { model | now = Just maybePosix }
            , Cmd.none
            )

        StartSignupFlow ->
            ( model
            , toBackend FetchPasskeyRegistrationOptions
            )

        GotPasskeyRegistrationResponse (Ok response) ->
            ( model
            , toBackend <| VerifyPasskeyRegistrationResponse response
            )

        GotPasskeyRegistrationResponse (Err err) ->
            Debug.log "Failed to decode passkey registration response" err
                |> always ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    createCredentialResponse GotPasskeyRegistrationResponse



-- VIEW


view :
    Request
    -> Model
    ->
        { page : View msg
        , toMsg : Msg -> msg
        }
    -> View msg
view _ model { page, toMsg } =
    { title = page.title
    , body =
        case model.data of
            RemoteData.NotAsked ->
                []

            RemoteData.Loading ->
                [ text "Loading..." ]

            RemoteData.Failure err ->
                [ text err ]

            RemoteData.Success data ->
                case data.currentUser of
                    Nothing ->
                        viewSignupButton toMsg :: page.body

                    Just user ->
                        text (Users.toString user.id) :: page.body
    , modal = page.modal
    }


viewSignupButton : (Msg -> msg) -> Html msg
viewSignupButton toMsg =
    Button.view (toMsg StartSignupFlow) [] [ text "Sign up" ]

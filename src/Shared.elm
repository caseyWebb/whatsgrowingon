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
import Data.PasskeyAuthenticationOptions exposing (PasskeyAuthenticationOptions)
import Data.PasskeyAuthenticationResponse exposing (PasskeyAuthenticationResponse)
import Data.Users as Users exposing (Passkey, User, UserId, Username)
import GenericDict as Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events exposing (onInput)
import Maybe.Extra as Maybe
import Passkey exposing (authenticate, onAuthenticationResponse, onRegistrationResponse, register)
import Random
import RemoteData exposing (RemoteData)
import Request exposing (Request)
import Slug exposing (Slug)
import Task
import Time
import Ui.Button as Button
import View exposing (View)


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
    , username : String
    }


init : { toBackend : ToBackend -> Cmd Msg } -> Request -> ( Model, Cmd Msg )
init { toBackend } _ =
    ( { data = RemoteData.Loading
      , now = Nothing
      , username = ""
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
    | StartSignupFlow String
    | GotPasskeyRegistrationResponse (Result String Passkey.RegistrationResponse)
    | StartLoginFlow
    | GotPasskeyAuthenticationResponse (Result String PasskeyAuthenticationResponse)
    | AttemptLogout
    | UpdateUsername String
    | NoOp


type ToBackend
    = FetchData
    | SaveZone Zone
    | DeleteZoneToBackend Slug
    | FetchPasskeyRegistrationOptions String
    | VerifyPasskeyRegistrationResponse Passkey.RegistrationResponse
    | FetchPasskeyAuthenticationOptions String
    | VerifyPasskeyAuthenticationResponse PasskeyAuthenticationResponse
    | Logout



-- | Logout


type ToFrontend
    = GotData
        { zones : Dict Slug Zone
        , crops : Dict Slug Crop
        , varieties : Dict Slug Variety
        , currentUser : Maybe User
        }
    | GotPasskeyRegistrationOptions Passkey.RegistrationOptions
    | GotPasskeyAuthenticationOptions (Result String PasskeyAuthenticationOptions)
    | Login User
    | LoggedOut


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

        FromBackend (GotPasskeyRegistrationOptions options) ->
            ( model, register options )

        FromBackend (GotPasskeyAuthenticationOptions options) ->
            ( model
            , Result.map authenticate options
                |> Result.mapError Debug.log
                |> Result.withDefault Cmd.none
            )

        FromBackend (Login user) ->
            ( { model
                | data =
                    RemoteData.map
                        (\data -> { data | currentUser = Just user })
                        model.data
              }
            , Cmd.none
            )

        FromBackend LoggedOut ->
            ( { model
                | data =
                    RemoteData.map
                        (\data -> { data | currentUser = Nothing })
                        model.data
              }
            , Cmd.none
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

        StartSignupFlow username ->
            ( model
            , toBackend (FetchPasskeyRegistrationOptions username)
            )

        GotPasskeyRegistrationResponse (Ok response) ->
            ( model
            , toBackend <| VerifyPasskeyRegistrationResponse response
            )

        GotPasskeyRegistrationResponse (Err err) ->
            Debug.log "Failed to decode passkey registration response" err
                |> always ( model, Cmd.none )

        StartLoginFlow ->
            ( model
            , toBackend (FetchPasskeyAuthenticationOptions model.username)
            )

        GotPasskeyAuthenticationResponse (Ok response) ->
            ( model
            , toBackend <| VerifyPasskeyAuthenticationResponse response
            )

        GotPasskeyAuthenticationResponse (Err err) ->
            Debug.log "Failed to decode passkey login response" err
                |> always ( model, Cmd.none )

        AttemptLogout ->
            ( model
            , toBackend Logout
            )

        UpdateUsername username ->
            ( { model | username = username }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ onRegistrationResponse GotPasskeyRegistrationResponse
        , onAuthenticationResponse GotPasskeyAuthenticationResponse
        ]



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
                        viewAuth toMsg model.username
                            :: page.body

                    Just user ->
                        text (Users.idToString user.id)
                            :: viewLogoutButton toMsg
                            :: page.body
    , modal = page.modal
    }


viewAuth : (Msg -> msg) -> String -> Html msg
viewAuth toMsg username =
    Html.div []
        [ label
            [ Attrs.for "username"
            ]
            [ text "Username" ]
        , input
            [ Attrs.type_ "text"
            , Attrs.name "username"
            , Attrs.attribute "autocomplete" "username webauthn"
            , Attrs.id "username"
            , Attrs.value username
            , onInput (toMsg << UpdateUsername)
            ]
            []
        , Button.view (toMsg StartLoginFlow)
            []
            [ text "Log in"
            ]
        , Button.view (toMsg (StartSignupFlow username))
            []
            [ text "Sign up"
            ]
        ]


viewLogoutButton : (Msg -> msg) -> Html msg
viewLogoutButton toMsg =
    Button.view (toMsg AttemptLogout)
        []
        [ text "Log out"
        ]

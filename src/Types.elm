module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Data exposing (..)
import Data.PasskeyRegistrationOptions exposing (PasskeyRegistrationOptions)
import Data.Users exposing (Passkey)
import Gen.Pages as Pages
import GenericDict exposing (Dict)
import Http
import Lamdera exposing (ClientId, SessionId)
import Shared
import Slug exposing (Slug)
import Url exposing (Url)


type alias FrontendModel =
    { url : Url
    , key : Key
    , shared : Shared.Model
    , page : Pages.Model
    }


type alias BackendModel =
    { zones : Dict Slug Zone
    , crops : Dict Slug Crop
    , varieties : Dict Slug Variety
    , users : Data.Users.Model
    , sessions : Dict SessionId Session
    , passkeyChallenges : Dict ClientId String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | Shared Shared.Msg
    | Page Pages.Msg
    | NoOpFrontendMsg


type ToBackend
    = SharedToBackend Shared.ToBackend
    | NoOpToBackend


type BackendMsg
    = ToFrontend ClientId ToFrontend
    | GotPasskeyRegistrationOptions ClientId (Result String ( String, PasskeyRegistrationOptions ))
    | GotPasskeyRegistrationResult SessionId ClientId (Result Http.Error Passkey)
    | NoOpBackendMsg


type ToFrontend
    = SharedToFrontend Shared.ToFrontend
    | NoOpToFrontend

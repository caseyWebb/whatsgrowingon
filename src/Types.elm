module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Data exposing (Zone)
import Gen.Pages as Pages
import GenericDict exposing (Dict)
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
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | Shared Shared.Msg
    | Page Pages.Msg
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend

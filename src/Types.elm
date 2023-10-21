module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Gen.Pages as Pages
import Shared
import Url exposing (Url)


type alias FrontendModel =
    { url : Url
    , key : Key
    , shared : Shared.Model
    , page : Pages.Model
    }


type alias BackendModel =
    { message : String
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

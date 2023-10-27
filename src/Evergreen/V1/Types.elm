module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V1.Data
import Evergreen.V1.Gen.Pages
import Evergreen.V1.Shared
import Evergreen.V1.Slug
import GenericDict
import Url


type alias FrontendModel =
    { url : Url.Url
    , key : Browser.Navigation.Key
    , shared : Evergreen.V1.Shared.Model
    , page : Evergreen.V1.Gen.Pages.Model
    }


type alias BackendModel =
    { zones : GenericDict.Dict Evergreen.V1.Slug.Slug Evergreen.V1.Data.Zone
    , crops : GenericDict.Dict Evergreen.V1.Slug.Slug Evergreen.V1.Data.Crop
    , varieties : GenericDict.Dict Evergreen.V1.Slug.Slug Evergreen.V1.Data.Variety
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Shared Evergreen.V1.Shared.Msg
    | Page Evergreen.V1.Gen.Pages.Msg
    | NoOpFrontendMsg


type ToBackend
    = SharedToBackend Evergreen.V1.Shared.ToBackend
    | NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = SharedToFrontend Evergreen.V1.Shared.ToFrontend
    | NoOpToFrontend

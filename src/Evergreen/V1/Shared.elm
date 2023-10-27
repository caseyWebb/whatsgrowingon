module Evergreen.V1.Shared exposing (..)

import Evergreen.V1.Data
import Evergreen.V1.RemoteData
import Evergreen.V1.Slug
import GenericDict
import Time


type alias Model =
    { data :
        Evergreen.V1.RemoteData.RemoteData
            String
            { zones : GenericDict.Dict Evergreen.V1.Slug.Slug Evergreen.V1.Data.Zone
            , crops : GenericDict.Dict Evergreen.V1.Slug.Slug Evergreen.V1.Data.Crop
            , varieties : GenericDict.Dict Evergreen.V1.Slug.Slug Evergreen.V1.Data.Variety
            }
    , now : Maybe Time.Posix
    }


type ToFrontend
    = GotData
        { zones : GenericDict.Dict Evergreen.V1.Slug.Slug Evergreen.V1.Data.Zone
        , crops : GenericDict.Dict Evergreen.V1.Slug.Slug Evergreen.V1.Data.Crop
        , varieties : GenericDict.Dict Evergreen.V1.Slug.Slug Evergreen.V1.Data.Variety
        }


type Msg
    = FromBackend ToFrontend
    | AddZone (Maybe Evergreen.V1.Slug.Slug)
    | DeleteZone Evergreen.V1.Slug.Slug
    | FocusZone Evergreen.V1.Slug.Slug
    | UpdateZone Bool Evergreen.V1.Data.Zone
    | GotCurrentTime Time.Posix
    | NoOp


type ToBackend
    = FetchData
    | SaveZone Evergreen.V1.Data.Zone
    | DeleteZoneToBackend Evergreen.V1.Slug.Slug

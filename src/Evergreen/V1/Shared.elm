module Evergreen.V1.Shared exposing (..)

import Evergreen.V1.Data
import Evergreen.V1.RemoteData
import Evergreen.V1.Slug
import GenericDict
import Time


type AddPlantingStep
    = AddPlantingModalStep1 Evergreen.V1.Slug.Slug
    | AddPlantingModalStep2 Evergreen.V1.Slug.Slug Evergreen.V1.Slug.Slug
    | AddPlantingModalStep3 Evergreen.V1.Slug.Slug Evergreen.V1.Slug.Slug Evergreen.V1.Slug.Slug Int


type Modal
    = AddPlantingModal AddPlantingStep


type alias Model =
    { data :
        Evergreen.V1.RemoteData.RemoteData
            String
            { zones : GenericDict.Dict Evergreen.V1.Slug.Slug Evergreen.V1.Data.Zone
            , crops : GenericDict.Dict Evergreen.V1.Slug.Slug Evergreen.V1.Data.Crop
            , varieties : GenericDict.Dict Evergreen.V1.Slug.Slug Evergreen.V1.Data.Variety
            }
    , modal : Maybe Modal
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
    | UpdateZone Bool Evergreen.V1.Data.Zone
    | ShowModal Modal
    | CloseModal
    | AdvanceAddPlantingModal AddPlantingStep
    | OnNewPlantingAmountChange String
    | AddPlanting Evergreen.V1.Slug.Slug Evergreen.V1.Slug.Slug Evergreen.V1.Slug.Slug Int (Maybe Time.Posix)
    | GotCurrentTime Time.Posix


type ToBackend
    = FetchData
    | SaveZone Evergreen.V1.Data.Zone

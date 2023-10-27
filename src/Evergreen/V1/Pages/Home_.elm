module Evergreen.V1.Pages.Home_ exposing (..)

import Evergreen.V1.Data
import Evergreen.V1.Slug
import Evergreen.V1.Ui.Modal
import Time


type AddPlantingStep
    = AddPlantingModalStep1 Evergreen.V1.Data.Zone
    | AddPlantingModalStep2 Evergreen.V1.Data.Zone Evergreen.V1.Data.Crop
    | AddPlantingModalStep3 Evergreen.V1.Data.Zone Evergreen.V1.Data.Crop Evergreen.V1.Data.Variety Int


type ModalKind
    = ConfirmDeleteZoneModal Evergreen.V1.Data.Zone
    | AddPlantingModal AddPlantingStep


type Msg
    = AddZone
    | UpdateZone Bool Evergreen.V1.Data.Zone
    | DeleteZone Evergreen.V1.Slug.Slug
    | ShowNewPlantingModal Evergreen.V1.Data.Zone
    | ShowConfirmDeleteZoneModal Evergreen.V1.Data.Zone
    | CloseModal
    | ModalMsg Evergreen.V1.Ui.Modal.Msg
    | FocusAddPlantingButton Evergreen.V1.Slug.Slug
    | FocusDeleteZoneModalButton Evergreen.V1.Slug.Slug
    | AdvanceAddPlantingModal AddPlantingStep
    | OnNewPlantingAmountChange String
    | AddPlanting Evergreen.V1.Slug.Slug Evergreen.V1.Slug.Slug Evergreen.V1.Slug.Slug Int (Maybe Time.Posix)
    | NoOp


type alias Model =
    { modal : Evergreen.V1.Ui.Modal.Model ModalKind Msg
    }

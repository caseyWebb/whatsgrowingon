module Modal exposing (Modal)

import Gen.Pages as Pages


type Modal
    = Modal Pages.Msg ModalKind


type alias ModalKind =
    ()



-- type ModalKind
--     = AddPlantingModal AddPlantingStep
--     | ConfirmDeleteZoneModal Zone

module Evergreen.V1.Pages.Home_ exposing (..)

import Evergreen.V1.Data
import Evergreen.V1.Slug


type alias Model =
    ()


type Msg
    = AddZone
    | UpdateZone Bool Evergreen.V1.Data.Zone
    | ShowNewPlantingModal Evergreen.V1.Slug.Slug

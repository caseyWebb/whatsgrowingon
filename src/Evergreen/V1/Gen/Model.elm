module Evergreen.V1.Gen.Model exposing (..)

import Evergreen.V1.Gen.Params.Home_
import Evergreen.V1.Gen.Params.NotFound
import Evergreen.V1.Pages.Home_


type Model
    = Redirecting_
    | Home_ Evergreen.V1.Gen.Params.Home_.Params Evergreen.V1.Pages.Home_.Model
    | NotFound Evergreen.V1.Gen.Params.NotFound.Params

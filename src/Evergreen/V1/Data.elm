module Evergreen.V1.Data exposing (..)

import Evergreen.V1.Slug
import Evergreen.V1.Ui.Color
import Time


type alias Planting =
    { cropId : Evergreen.V1.Slug.Slug
    , varietyId : Evergreen.V1.Slug.Slug
    , amount : Int
    , time : Time.Posix
    }


type alias Zone =
    { slug : Evergreen.V1.Slug.Slug
    , index : Int
    , name : String
    , plantings : List Planting
    }


type alias Crop =
    { slug : Evergreen.V1.Slug.Slug
    , name : String
    , varieties : List Evergreen.V1.Slug.Slug
    , color : Evergreen.V1.Ui.Color.Color
    }


type alias Variety =
    { slug : Evergreen.V1.Slug.Slug
    , name : String
    , color : Maybe Evergreen.V1.Ui.Color.Color
    }

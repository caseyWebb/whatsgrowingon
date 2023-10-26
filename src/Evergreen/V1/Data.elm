module Evergreen.V1.Data exposing (..)

import Evergreen.V1.Slug
import Time


type alias Planting =
    { cropId : Evergreen.V1.Slug.Slug
    , varietyId : Evergreen.V1.Slug.Slug
    , amount : Float
    , time : Time.Posix
    , notes : List ( Time.Posix, String )
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
    }


type alias Variety =
    { slug : Evergreen.V1.Slug.Slug
    , name : String
    }

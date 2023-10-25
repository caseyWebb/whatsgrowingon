module Data exposing (..)

import Slug exposing (Slug)
import Time


type alias Zone =
    { slug : Slug
    , index : Int
    , name : String
    , plantings : List Planting
    }


type alias Planting =
    { time : Time.Posix
    , notes : List ( Time.Posix, String )
    , cropId : Slug
    , varietyId : Slug
    }


type alias Crop =
    { slug : Slug
    , name : String
    , varietyId : Slug
    }


type alias Variety =
    { name : String
    , daysToHarvest : Int
    }

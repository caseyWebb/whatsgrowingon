module Data exposing (..)

import Slug exposing (Slug)
import Time


type alias Zone =
    { slug : Slug
    , name : String
    , plantings : List Planting
    }


type alias Crop =
    { slug : Slug
    , name : String
    , variety : Variety
    }


type alias Variety =
    { name : String
    , daysToHarvest : Int
    }


type alias Planting =
    { time : Time.Posix
    , crop : Crop
    , variety : Variety
    , notes : List ( Time.Posix, String )
    }

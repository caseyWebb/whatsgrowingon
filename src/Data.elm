module Data exposing (..)

import Slug exposing (Slug)
import Time
import Ui.Color exposing (Color)


type alias Zone =
    { slug : Slug
    , index : Int
    , name : String
    , plantings : List Planting
    }


type alias Planting =
    { cropId : Slug
    , varietyId : Slug
    , amount : Int
    , time : Time.Posix
    }


type alias Crop =
    { slug : Slug
    , name : String
    , varieties : List Slug
    , color : Color
    }


type alias Variety =
    { slug : Slug
    , name : String
    , color : Maybe Color
    }


type alias Note =
    { time : Time.Posix
    , text : String
    , zoneId : Maybe Slug
    , plantingId : Maybe Slug
    , cropId : Maybe Slug
    , varietyId : Maybe Slug
    }

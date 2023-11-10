module COSE exposing (..)

import COSE.Algorithm exposing (Algorithm)


type KeyType
    = EC2


type alias CoseKey =
    { keyType : KeyType
    , algorithm : Algorithm

    -- , keyId : String
    -- , key : String
    }

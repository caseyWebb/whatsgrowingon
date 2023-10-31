module COSE exposing (..)


type KeyType
    = EC2


type Algorithm
    = ES256
    | ES384
    | ES512


type alias CoseKey =
    { keyType : KeyType
    , algorithm : Algorithm

    -- , keyId : String
    -- , key : String
    }

module Gen.Types exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode


type alias PasskeyAuthenticationOptions =
    { rpID : String
    }


passkeyAuthenticationOptionsDecoder : Decode.Decoder PasskeyAuthenticationOptions
passkeyAuthenticationOptionsDecoder =
    Decode.map PasskeyAuthenticationOptions
        (Decode.oneOf [ Decode.field "rpID" Decode.string ])


passkeyAuthenticationOptionsEncoder : PasskeyAuthenticationOptions -> Encode.Value
passkeyAuthenticationOptionsEncoder passkeyAuthenticationOptions =
    Encode.object
        [ ( "rpID", Encode.string passkeyAuthenticationOptions.rpID )
        ]

module Data.PasskeyRegistrationResponse exposing
    ( PasskeyRegistrationResponse
    , decoder
    , encoder
    , fromString
    )

import Json.Decode as Decode
import Json.Encode as Encode


type PasskeyRegistrationResponse
    = PasskeyRegistrationResponse String


decoder : Decode.Decoder PasskeyRegistrationResponse
decoder =
    Decode.string |> Decode.map PasskeyRegistrationResponse


encoder : PasskeyRegistrationResponse -> Encode.Value
encoder (PasskeyRegistrationResponse passkey) =
    Encode.string passkey


fromString : String -> PasskeyRegistrationResponse
fromString =
    PasskeyRegistrationResponse

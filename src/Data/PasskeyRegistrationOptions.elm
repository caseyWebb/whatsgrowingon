module Data.PasskeyRegistrationOptions exposing
    ( PasskeyRegistrationOptions
    , decoder
    , encoder
    , fromString
    )

import Json.Decode as Decode
import Json.Encode as Encode


type PasskeyRegistrationOptions
    = PasskeyRegistrationOptions String


decoder : Decode.Decoder PasskeyRegistrationOptions
decoder =
    Decode.string |> Decode.map PasskeyRegistrationOptions


encoder : PasskeyRegistrationOptions -> Encode.Value
encoder (PasskeyRegistrationOptions passkey) =
    Encode.string passkey


fromString : String -> PasskeyRegistrationOptions
fromString =
    PasskeyRegistrationOptions

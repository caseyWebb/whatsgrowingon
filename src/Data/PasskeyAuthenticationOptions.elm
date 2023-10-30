module Data.PasskeyAuthenticationOptions exposing
    ( PasskeyAuthenticationOptions
    , decoder
    , encoder
    , fromString
    )

import Json.Decode as Decode
import Json.Encode as Encode


type PasskeyAuthenticationOptions
    = PasskeyAuthenticationOptions String


decoder : Decode.Decoder PasskeyAuthenticationOptions
decoder =
    Decode.string |> Decode.map PasskeyAuthenticationOptions


encoder : PasskeyAuthenticationOptions -> Encode.Value
encoder (PasskeyAuthenticationOptions passkey) =
    Encode.string passkey


fromString : String -> PasskeyAuthenticationOptions
fromString =
    PasskeyAuthenticationOptions

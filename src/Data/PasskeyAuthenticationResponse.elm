module Data.PasskeyAuthenticationResponse exposing
    ( PasskeyAuthenticationResponse
    , credentialId
    , decoder
    , encoder
    )

import Json.Decode as Decode
import Json.Encode as Encode


type Raw
    = Raw String


type PasskeyAuthenticationResponse
    = PasskeyAuthenticationResponse Raw { id : String }


decoder : Decode.Decoder PasskeyAuthenticationResponse
decoder =
    Decode.string
        |> Decode.andThen
            (\raw ->
                Decode.decodeString (Decode.field "id" Decode.string) raw
                    |> (\result ->
                            case result of
                                Ok id ->
                                    Decode.succeed (PasskeyAuthenticationResponse (Raw raw) { id = id })

                                Err _ ->
                                    Decode.fail "Failed to decode PasskeyAuthenticationResponse"
                       )
            )


encoder : PasskeyAuthenticationResponse -> Encode.Value
encoder (PasskeyAuthenticationResponse (Raw raw) _) =
    Encode.string raw


credentialId : PasskeyAuthenticationResponse -> String
credentialId (PasskeyAuthenticationResponse _ { id }) =
    id

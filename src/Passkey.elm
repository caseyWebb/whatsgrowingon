port module Passkey exposing
    ( authenticate
    , onAuthenticationResponse
    , onRegistrationResponse
    , register
    )

import Data.PasskeyAuthenticationOptions as PasskeyAuthenticationOptions exposing (PasskeyAuthenticationOptions)
import Data.PasskeyAuthenticationResponse as PasskeyAuthenticationResponse exposing (PasskeyAuthenticationResponse)
import Data.PasskeyRegistrationOptions as PasskeyRegistrationOptions exposing (PasskeyRegistrationOptions)
import Data.PasskeyRegistrationResponse as PasskeyRegistrationResponse exposing (PasskeyRegistrationResponse)
import Data.Users as Users exposing (Passkey, Username, passkeyDecoder)
import Json.Decode as Decode
import Json.Encode as Encode


port registerPasskeyPort : Encode.Value -> Cmd msg


port passkeyRegistrationResponsePort : (Encode.Value -> msg) -> Sub msg


port authenticatePasskeyPort : Encode.Value -> Cmd msg


port passkeyAuthenticationResponsePort : (Encode.Value -> msg) -> Sub msg


register : PasskeyRegistrationOptions -> Cmd msg
register =
    PasskeyRegistrationOptions.encoder >> registerPasskeyPort


onRegistrationResponse : (Result String ( Username, PasskeyRegistrationResponse ) -> msg) -> Sub msg
onRegistrationResponse toMsg =
    passkeyRegistrationResponsePort
        (Decode.decodeValue
            (Decode.map2
                Tuple.pair
                (Decode.field "username" Users.usernameDecoder)
                (Decode.field "response" PasskeyRegistrationResponse.decoder)
            )
            >> Result.mapError Decode.errorToString
            >> toMsg
        )


authenticate : PasskeyAuthenticationOptions -> Cmd msg
authenticate =
    PasskeyAuthenticationOptions.encoder >> authenticatePasskeyPort


onAuthenticationResponse : (Result String PasskeyAuthenticationResponse -> msg) -> Sub msg
onAuthenticationResponse toMsg =
    passkeyAuthenticationResponsePort
        (Decode.decodeValue
            (Decode.field "response" PasskeyAuthenticationResponse.decoder)
            >> Result.mapError Decode.errorToString
            >> toMsg
        )

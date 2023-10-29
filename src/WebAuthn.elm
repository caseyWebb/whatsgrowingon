port module WebAuthn exposing (createCredential, createCredentialResponse)

import Data.PasskeyRegistrationOptions as PasskeyRegistrationOptions exposing (PasskeyRegistrationOptions)
import Data.PasskeyRegistrationResponse as PasskeyRegistrationResponse exposing (PasskeyRegistrationResponse)
import Json.Decode as Decode
import Json.Encode as Encode


port createCredentialPort : Encode.Value -> Cmd msg


port createCredentialResponsePort : (Encode.Value -> msg) -> Sub msg


createCredential : PasskeyRegistrationOptions -> Cmd msg
createCredential =
    PasskeyRegistrationOptions.encoder >> createCredentialPort


createCredentialResponse : (Result String PasskeyRegistrationResponse -> msg) -> Sub msg
createCredentialResponse toMsg =
    createCredentialResponsePort
        (Decode.decodeValue PasskeyRegistrationResponse.decoder
            >> Result.mapError Decode.errorToString
            >> toMsg
        )

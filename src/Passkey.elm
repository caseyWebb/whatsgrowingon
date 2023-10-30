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
import Random


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


type alias RegistrationOptions =
    { challenge : String
    , rp : RelyingParty
    , user : User
    , pubKeyCredParams :
        List
            { alg : Int
            , type_ : String
            }
    , timeout : Int
    , attestationType : AttestationType
    , excludeCredentials :
        List
            { id : String
            , type_ : String
            , transports : List String
            }
    , authenticatorSelection :
        { authenticatorAttachment : String
        , requireResidentKey : Bool
        , residentKey : Maybe String
        , userVerification : String
        }

    -- extensions : {
    --     appid : String
    --     appidExclude : String
    --     uvm : Bool
    -- }
    }


defaultRegistrationOptions =
    { challenge = ""
    , rp =
        { name = ""
        , id = ""
        }
    , user =
        { id = ""
        , name = ""
        , displayName = ""
        }
    , pubKeyCredParams = []
    , timeout = 60000
    , attestationType = None
    , excludeCredentials = []
    , authenticatorSelection =
        { authenticatorAttachment = "cross-platform"
        , requireResidentKey = False
        , residentKey = Nothing
        , userVerification = "preferred"
        }
    }


type AttestationType
    = None
    | Indirect
    | Direct


type alias RelyingParty =
    { name : String
    , id : String
    }


type alias User =
    { id : String
    , name : String
    , displayName : String
    }


type RegistrationOption
    = RegistrationOption (RegistrationOptions -> RegistrationOptions)


requireUserVerification : RegistrationOption
requireUserVerification =
    RegistrationOption
        (\options ->
            { options
                | authenticatorSelection =
                    options.authenticatorSelection
                        |> (\authenticatorSelection ->
                                { authenticatorSelection | userVerification = "required" }
                           )
            }
        )


generateRegistrationOptions : { rp : RelyingParty, user : User } -> List RegistrationOption -> (RegistrationOptions -> msg) -> Cmd msg
generateRegistrationOptions requiredSettings optionalSettings onGotRegistrationOptions =
    let
        withChallenge challenge =
            RegistrationOption (\options -> { options | challenge = challenge })

        -- settings = List.foldl (\(RegistrationOption f) options -> f options) defaultRegistrationOptions optionalSettings
    in
    Random.generate
        (\challenge ->
            onGotRegistrationOptions
                (List.foldl (\(RegistrationOption f) options -> f options) defaultRegistrationOptions optionalSettings
                    |> withChallenge challenge
                )
        )
        (Random.string 32)

port module Passkey exposing
    ( RegistrationOptions
    , RegistrationResponse
    , allowedAlgorithms
    , authenticate
    , excludeCredentials
    , generateRegistrationOptions
    , onAuthenticationResponse
    , onRegistrationResponse
    , register
    , registrationOptionsEncoder
    , registrationsOptionsChallenge
    , requireUserVerification
    )

import Base64.Decode
import Base64.Encode
import Bytes exposing (Bytes)
import Bytes.Encode
import Cbor.Decode as Cbor
import Data.PasskeyAuthenticationOptions as PasskeyAuthenticationOptions exposing (PasskeyAuthenticationOptions)
import Data.PasskeyAuthenticationResponse as PasskeyAuthenticationResponse exposing (PasskeyAuthenticationResponse)
import Html.Attributes exposing (type_)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Random
import Random.String as Random


port registerPasskeyPort : Encode.Value -> Cmd msg


port passkeyRegistrationResponsePort : (Encode.Value -> msg) -> Sub msg


port authenticatePasskeyPort : Encode.Value -> Cmd msg


port passkeyAuthenticationResponsePort : (Encode.Value -> msg) -> Sub msg


register : RegistrationOptions -> Cmd msg
register =
    registrationOptionsEncoder >> registerPasskeyPort


onRegistrationResponse : (Result String RegistrationResponse -> msg) -> Sub msg
onRegistrationResponse toMsg =
    passkeyRegistrationResponsePort
        (Decode.decodeValue registrationResponseDecoder
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


type RegistrationOptions
    = RegistrationOptions RegistrationOptionsInternal


type alias RegistrationOptionsInternal =
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
        { authenticatorAttachment : Maybe String
        , requireResidentKey : Maybe Bool
        , residentKey : Maybe String
        , userVerification : String
        }

    -- extensions : {
    --     appid : String
    --     appidExclude : String
    --     uvm : Bool
    -- }
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
    = RegistrationOption (RegistrationOptionsInternal -> RegistrationOptionsInternal)


generateRegistrationOptions : { rp : RelyingParty, user : User } -> List RegistrationOption -> (RegistrationOptions -> msg) -> Cmd msg
generateRegistrationOptions { rp, user } optionalSettings onGotRegistrationOptions =
    let
        updateAuthenticatorSelection f options =
            { options | authenticatorSelection = f options.authenticatorSelection }

        buildOptions : String -> RegistrationOptionsInternal
        buildOptions challenge =
            List.foldl (\(RegistrationOption f) options -> f options)
                { challenge = challenge
                , rp = rp
                , user = user
                , pubKeyCredParams = []
                , timeout = 60000
                , attestationType = None
                , excludeCredentials = []
                , authenticatorSelection =
                    { authenticatorAttachment = Nothing
                    , requireResidentKey = Nothing
                    , residentKey = Just "preferred"
                    , userVerification = "preferred"
                    }
                }
                optionalSettings
                |> (\options ->
                        case
                            ( options.authenticatorSelection.residentKey
                            , options.authenticatorSelection.requireResidentKey
                            )
                        of
                            ( Nothing, Just True ) ->
                                updateAuthenticatorSelection
                                    (\as_ -> { as_ | residentKey = Just "required" })
                                    options

                            ( Nothing, Just False ) ->
                                updateAuthenticatorSelection
                                    (\as_ -> { as_ | residentKey = Just "discouraged" })
                                    options

                            ( Just residentKey, _ ) ->
                                updateAuthenticatorSelection
                                    (\as_ -> { as_ | requireResidentKey = Just <| residentKey == "required" })
                                    options

                            ( Nothing, Nothing ) ->
                                options
                   )

        challengeGenerator : Random.Generator String
        challengeGenerator =
            Random.list 32 (Random.int 0 255 |> Random.map Bytes.Encode.unsignedInt8)
                |> Random.map
                    (Bytes.Encode.sequence
                        >> Bytes.Encode.encode
                        >> Base64.Encode.bytes
                        >> Base64.Encode.encode
                    )
    in
    Random.generate (buildOptions >> RegistrationOptions >> onGotRegistrationOptions) challengeGenerator


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


allowedAlgorithms : List Int -> RegistrationOption
allowedAlgorithms algorithms =
    RegistrationOption
        (\options ->
            { options
                | pubKeyCredParams =
                    algorithms
                        |> List.map
                            (\alg ->
                                { alg = alg
                                , type_ = "public-key"
                                }
                            )
            }
        )


excludeCredentials : List String -> RegistrationOption
excludeCredentials ids =
    RegistrationOption
        (\options ->
            { options
                | excludeCredentials =
                    ids
                        |> List.map
                            (\id ->
                                { id = id
                                , type_ = "public-key"
                                , transports = []
                                }
                            )
            }
        )


registrationsOptionsChallenge : RegistrationOptions -> String
registrationsOptionsChallenge (RegistrationOptions options) =
    options.challenge


registrationOptionsEncoder : RegistrationOptions -> Encode.Value
registrationOptionsEncoder (RegistrationOptions options) =
    let
        attestationTypeEncoder attestationType =
            case attestationType of
                None ->
                    "none"

                Indirect ->
                    "indirect"

                Direct ->
                    "direct"

        optionalProperty encoder k maybeV =
            Maybe.cons (Maybe.map (encoder >> Tuple.pair k) maybeV)
    in
    Encode.object
        [ ( "challenge", Encode.string options.challenge )
        , ( "rp"
          , Encode.object
                [ ( "name", Encode.string options.rp.name )
                , ( "id", Encode.string options.rp.id )
                ]
          )
        , ( "user"
          , Encode.object
                [ ( "id", Encode.string options.user.id )
                , ( "name", Encode.string options.user.name )
                , ( "displayName", Encode.string options.user.displayName )
                ]
          )
        , ( "pubKeyCredParams"
          , Encode.list
                (\{ alg, type_ } ->
                    Encode.object
                        [ ( "alg", Encode.int alg )
                        , ( "type", Encode.string type_ )
                        ]
                )
                options.pubKeyCredParams
          )
        , ( "timeout", Encode.int options.timeout )
        , ( "attestation", Encode.string (options.attestationType |> attestationTypeEncoder) )
        , ( "excludeCredentials"
          , Encode.list
                (\{ id, type_, transports } ->
                    Encode.object
                        [ ( "id", Encode.string id )
                        , ( "type", Encode.string type_ )
                        , ( "transports", Encode.list Encode.string transports )
                        ]
                )
                options.excludeCredentials
          )
        , ( "authenticatorSelection"
          , Encode.object
                ([ ( "userVerification", Encode.string options.authenticatorSelection.userVerification )
                 ]
                    |> optionalProperty Encode.string "authenticatorAttachment" options.authenticatorSelection.authenticatorAttachment
                    |> optionalProperty Encode.bool "requireResidentKey" options.authenticatorSelection.requireResidentKey
                    |> optionalProperty Encode.string "residentKey" options.authenticatorSelection.residentKey
                )
          )
        ]


type RegistrationResponse
    = RegistrationResponse RegistrationResponseInternal


type alias RegistrationResponseInternal =
    { id : String
    , credentialType : String
    , attestationObject : AttestationObject
    , clientData : ClientData
    }


type alias AttestationObject =
    { fmt : String

    -- , authData : Bytes
    -- , authData : AuthData
    -- , attStmt : String
    }


type alias ClientData =
    { type_ : String
    , challenge : String
    , origin : String
    , tokenBinding : Maybe TokenBinding
    }


type alias TokenBinding =
    { status : TokenBindingStatus
    , id : String
    }


type TokenBindingStatus
    = Present
    | Supported
    | NotSupported


registrationResponseDecoder : Decode.Decoder RegistrationResponse
registrationResponseDecoder =
    -- These are not 1:1 with the browser API because data has to be encoded
    -- from binary formats to send over ports. Additionally clientDataJSON is
    -- decoded on the Javascript side because... it's easier.
    Decode.map RegistrationResponse <|
        Decode.map4 RegistrationResponseInternal
            (Decode.field "id" Decode.string)
            (Decode.field "type" Decode.string)
            (Decode.at [ "response", "attestationObject" ] <|
                (Decode.string
                    |> Decode.andThen
                        (\base64Str ->
                            case Base64.Decode.decode Base64.Decode.bytes base64Str of
                                Ok cborBytes ->
                                    Cbor.decode
                                        (Cbor.record Cbor.string
                                            AttestationObject
                                            (Cbor.fields
                                                >> Cbor.field "fmt" Cbor.string
                                             -- >> Cbor.field "attStmt"
                                             --     (Cbor.record Cbor.string
                                             --      -- (Cbor.fields
                                             --      --     >> Cbor.field "sig" Cbor.string
                                             --      --     >> Cbor.field "x5c"
                                             --      --         (Cbor.list Cbor.string)
                                             --      -- )
                                             --     )
                                             -- >> Cbor.field "authData" Cbor.bytes
                                            )
                                        )
                                        cborBytes
                                        |> Maybe.map Decode.succeed
                                        |> Maybe.withDefault (Decode.fail "Failed to decode attestation cbor")

                                Err _ ->
                                    Decode.fail "Invalid attestation object"
                        )
                )
            )
            (Decode.at [ "response", "clientDataJSON" ] <|
                Decode.map4 ClientData
                    (Decode.field "type" Decode.string)
                    (Decode.field "challenge" Decode.string)
                    (Decode.field "origin" Decode.string)
                    (Decode.maybe <|
                        Decode.field "tokenBinding" <|
                            Decode.map2 TokenBinding
                                (Decode.field "status"
                                    (Decode.string
                                        |> Decode.andThen
                                            (\status ->
                                                case status of
                                                    "present" ->
                                                        Decode.succeed Present

                                                    "supported" ->
                                                        Decode.succeed Supported

                                                    "not-supported" ->
                                                        Decode.succeed NotSupported

                                                    _ ->
                                                        Decode.fail "Invalid token binding status"
                                            )
                                    )
                                )
                                (Decode.field "id" Decode.string)
                    )
            )


verifyRegistrationResponse : RegistrationOptions -> RegistrationResponse -> { expectedOrigin : String } -> Result String ()
verifyRegistrationResponse (RegistrationOptions options) (RegistrationResponse response) { expectedOrigin } =
    if response.credentialType /= "public-key" then
        Err "Invalid credential type"

    else if response.clientData.type_ /= "webauthn.create" then
        Err "Invalid client data type"

    else if response.clientData.challenge /= options.challenge then
        Err "Invalid challenge"

    else if response.clientData.origin /= expectedOrigin then
        Err "Invalid origin"

    else
        Ok ()

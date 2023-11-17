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

import Base64
import Bitwise as Bits
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import COSE exposing (CoseKey)
import COSE.Algorithm as Algorithm exposing (Algorithm)
import COSE.Decode as COSE
import Cbor.Decode as Cbor
import Data.PasskeyAuthenticationOptions as PasskeyAuthenticationOptions exposing (PasskeyAuthenticationOptions)
import Data.PasskeyAuthenticationResponse as PasskeyAuthenticationResponse exposing (PasskeyAuthenticationResponse)
import Html.Attributes exposing (type_)
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Maybe.Extra as Maybe
import Random
import Random.String as Random
import SHA256
import Tuple.Extra exposing (triple)


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
            { alg : Algorithm
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
        , userVerification : UserVerification
        }
    }


type UserVerification
    = Required
    | Preferred
    | Discouraged


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


generateRegistrationOptions :
    { rp : RelyingParty
    , user :
        { name : String
        , displayName : String
        }
    }
    -> List RegistrationOption
    -> (Result String RegistrationOptions -> msg)
    -> Cmd msg
generateRegistrationOptions { rp, user } optionalSettings onGotRegistrationOptions =
    let
        updateAuthenticatorSelection f options =
            { options | authenticatorSelection = f options.authenticatorSelection }

        buildOptions : ( String, String ) -> RegistrationOptionsInternal
        buildOptions ( challenge, userId ) =
            List.foldl (\(RegistrationOption f) options -> f options)
                { challenge = challenge
                , rp = rp
                , user =
                    { id = userId
                    , name = user.name
                    , displayName = user.displayName
                    }
                , pubKeyCredParams = []
                , timeout = 60000
                , attestationType = None
                , excludeCredentials = []
                , authenticatorSelection =
                    { authenticatorAttachment = Nothing
                    , requireResidentKey = Nothing
                    , residentKey = Just "preferred"
                    , userVerification = Preferred
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

        challengeGenerator : Random.Generator (Maybe String)
        challengeGenerator =
            Random.list 32 (Random.int 0 255 |> Random.map Bytes.Encode.unsignedInt8)
                |> Random.map
                    (Bytes.Encode.sequence
                        >> Bytes.Encode.encode
                        >> Base64.fromBytes
                    )

        userIdGenerator : Random.Generator (Maybe String)
        userIdGenerator =
            Random.list 8 (Random.int 0 255 |> Random.map Bytes.Encode.unsignedInt8)
                |> Random.map
                    (Bytes.Encode.sequence
                        >> Bytes.Encode.encode
                        >> Base64.fromBytes
                    )
    in
    Random.map2
        (\challenge userId -> Maybe.map2 Tuple.pair challenge userId)
        challengeGenerator
        userIdGenerator
        |> Random.generate
            (Maybe.map (buildOptions >> RegistrationOptions >> Ok)
                >> Maybe.withDefault (Err "Error generating challenge and/or user id")
                >> onGotRegistrationOptions
            )


requireUserVerification : RegistrationOption
requireUserVerification =
    RegistrationOption
        (\options ->
            { options
                | authenticatorSelection =
                    options.authenticatorSelection
                        |> (\authenticatorSelection ->
                                { authenticatorSelection | userVerification = Required }
                           )
            }
        )


allowedAlgorithms : List Algorithm -> RegistrationOption
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
        encodeAttestationType attestationType =
            Encode.string <|
                case attestationType of
                    None ->
                        "none"

                    Indirect ->
                        "indirect"

                    Direct ->
                        "direct"

        encodeUserVerification userVerification =
            Encode.string <|
                case userVerification of
                    Required ->
                        "required"

                    Preferred ->
                        "preferred"

                    Discouraged ->
                        "discouraged"

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
                        [ ( "alg", Algorithm.encodeJson alg )
                        , ( "type", Encode.string type_ )
                        ]
                )
                options.pubKeyCredParams
          )
        , ( "timeout", Encode.int options.timeout )
        , ( "attestation", encodeAttestationType options.attestationType )
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
                ([ ( "userVerification", encodeUserVerification options.authenticatorSelection.userVerification )
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
    , clientDataJSON : String -- Raw JSON
    }


type alias AttestationObject =
    { authData : Bytes
    , attStmt : Maybe AttestationStatement
    }


type AttestationStatement
    = Packed { alg : Algorithm, sig : Bytes, x5c : Maybe (List Bytes) }


type alias AuthenticationData =
    { rpIdHash : Bytes
    , flags :
        { up : Bool -- User Present
        , uv : Bool -- User Verified
        , be : Bool -- Backup Eligible
        , bs : Bool -- Backup Status
        , at : Bool -- Attestation Data Included
        , ed : Bool -- Extension Data Included
        , flags : Int
        }
    , counter : Int
    , attestedCredentialData :
        Maybe
            { aaguid : Bytes
            , credentialId : Bytes
            , publicKey : CoseKey
            }
    }


type alias ClientData =
    { type_ : String
    , challenge : String
    , origin : String
    , topOrigin : Maybe String
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
    let
        base64EncodedCborDecoder decoder =
            Decode.string
                |> Decode.andThen
                    (Base64.fromString
                        >> Maybe.andThen Base64.toBytes
                        >> Maybe.map (Cbor.decode decoder >> Maybe.withDefault (Decode.fail "Failed to decode cbor"))
                        >> Maybe.withDefault (Decode.fail "Invalid base64")
                    )

        attestationObjectDecoder =
            base64EncodedCborDecoder
                (Cbor.record Cbor.string
                    (\fmt attStmtBytes authData ->
                        let
                            attStmtResult =
                                Cbor.decode (attestationStatementDecoder fmt) attStmtBytes
                                    |> Maybe.withDefault (Err "Failed to decode attestation statement")
                        in
                        case Result.map (AttestationObject authData) attStmtResult of
                            Ok attestationObject ->
                                Decode.succeed attestationObject

                            Err err ->
                                Decode.fail err
                    )
                    (Cbor.fields
                        >> Cbor.field "fmt" Cbor.string
                        >> Cbor.field "attStmt" Cbor.bytes
                        >> Cbor.field "authData" Cbor.bytes
                    )
                )

        attestationStatementDecoder fmt =
            case fmt of
                "packed" ->
                    Cbor.record Cbor.string
                        (\alg sig x5c -> Ok (Just (Packed { alg = alg, sig = sig, x5c = x5c })))
                        (Cbor.fields
                            >> Cbor.field "alg" Algorithm.cborDecoder
                            >> Cbor.field "sig" Cbor.bytes
                            >> Cbor.optionalField "x5c" (Cbor.list Cbor.bytes)
                        )

                "none" ->
                    Cbor.succeed (Ok Nothing)

                _ ->
                    Cbor.succeed (Err "Unsupported attestation format")
    in
    -- Note: Array Buffers can't be sent through ports, so those are encoded as base64 strings
    Decode.map
        RegistrationResponse
    <|
        Decode.map4 RegistrationResponseInternal
            (Decode.field "id" Decode.string)
            (Decode.field "type" Decode.string)
            (Decode.at [ "response", "attestationObject" ] attestationObjectDecoder)
            (Decode.at [ "response", "clientDataJSON" ] Decode.string)


clientDataDecoder : Decode.Decoder ClientData
clientDataDecoder =
    let
        tokenBindingStatusDecoder =
            Decode.string
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
    in
    Decode.map5 ClientData
        (Decode.field "type" Decode.string)
        (Decode.field "challenge" Decode.string)
        (Decode.field "origin" Decode.string)
        (Decode.maybe <| Decode.field "topOrigin" Decode.string)
        (Decode.maybe <|
            Decode.field "tokenBinding" <|
                Decode.map2 TokenBinding
                    (Decode.field "status" tokenBindingStatusDecoder)
                    (Decode.field "id" Decode.string)
        )


{-| <https://w3c.github.io/webauthn/#sctn-authenticator-data>
-}
authDataDecoder : Bytes.Decode.Decoder (Result String AuthenticationData)
authDataDecoder =
    Bytes.Decode.map3 triple
        -- rpIdHash
        (Bytes.Decode.bytes 32)
        -- flags
        (Bytes.Decode.unsignedInt8
            |> Bytes.Decode.map
                (\flags ->
                    { up = Bits.and flags (1 |> Bits.shiftLeftBy 0) /= 0
                    , uv = Bits.and flags (1 |> Bits.shiftLeftBy 2) /= 0
                    , be = Bits.and flags (1 |> Bits.shiftLeftBy 3) /= 0
                    , bs = Bits.and flags (1 |> Bits.shiftLeftBy 4) /= 0
                    , at = Bits.and flags (1 |> Bits.shiftLeftBy 6) /= 0
                    , ed = Bits.and flags (1 |> Bits.shiftLeftBy 7) /= 0
                    , flags = flags
                    }
                )
        )
        -- counter
        (Bytes.Decode.unsignedInt32 Bytes.BE)
        |> Bytes.Decode.andThen
            (\( rpIdHash, flags, counter ) ->
                if flags.at then
                    Bytes.Decode.map3
                        (\aaguid credentialId ->
                            Result.map
                                (\publicKey ->
                                    { rpIdHash = rpIdHash
                                    , flags = flags
                                    , counter = counter
                                    , attestedCredentialData =
                                        Just
                                            { aaguid = aaguid
                                            , credentialId = credentialId
                                            , publicKey = publicKey
                                            }
                                    }
                                )
                        )
                        {-
                           https://w3c.github.io/webauthn/#sctn-attested-credential-data
                        -}
                        -- aaguid
                        (Bytes.Decode.bytes 16)
                        -- credentialIdLength -> credentialId
                        (Bytes.Decode.unsignedInt16 Bytes.BE |> Bytes.Decode.andThen Bytes.Decode.bytes)
                        -- publicKey
                        (Cbor.runDecoder COSE.decoder)

                else
                    Bytes.Decode.succeed
                        (Ok
                            { rpIdHash = rpIdHash
                            , flags = flags
                            , counter = counter
                            , attestedCredentialData = Nothing
                            }
                        )
            )


verifyRegistrationResponse :
    { expectedOrigin : String }
    -> RegistrationOptions
    -> RegistrationResponse
    -> Result String RegistrationResponse
verifyRegistrationResponse { expectedOrigin } (RegistrationOptions options) (RegistrationResponse response) =
    let
        clientDataHash =
            SHA256.fromString response.clientDataJSON

        clientDataResult =
            Decode.decodeString clientDataDecoder response.clientDataJSON
                |> Result.mapError Decode.errorToString

        authDataResult =
            Bytes.Decode.decode authDataDecoder response.attestationObject.authData
                |> Maybe.withDefault (Err "Failed to decode auth data")

        expectedRpIdHash =
            SHA256.fromString options.rp.id |> SHA256.toBytes

        isPermittedAlgorithm alg =
            options.pubKeyCredParams
                |> List.map .alg
                |> List.member alg
    in
    Result.map2 Tuple.pair clientDataResult authDataResult
        |> Result.andThen
            (\( clientData, authData ) ->
                if response.credentialType /= "public-key" then
                    Err "Invalid credential type"

                else if clientData.type_ /= "webauthn.create" then
                    Err "Invalid client data type"

                else if clientData.challenge /= options.challenge then
                    Err "Invalid challenge"

                else if clientData.origin /= expectedOrigin then
                    Err "Invalid origin"

                else if clientData.topOrigin /= Nothing then
                    Err "Invalid top origin"

                else if authData.rpIdHash /= expectedRpIdHash then
                    Err "Invalid rpIdHash"

                else if not authData.flags.up then
                    Err "User presence is required"

                else if options.authenticatorSelection.userVerification == Required && not authData.flags.uv then
                    Err "User verification is required"

                else if not authData.flags.be && authData.flags.bs then
                    Err "Invalid backup status"

                else if
                    not
                        (authData.attestedCredentialData
                            |> Maybe.map (.publicKey >> .algorithm >> isPermittedAlgorithm)
                            |> Maybe.withDefault True
                        )
                then
                    Err "Invalid credential algorithm"

                else
                    case response.attestationObject.attStmt of
                        Just (Packed { alg, sig, x5c }) ->
                            Debug.todo ""

                        Nothing ->
                            Err "Invalid attestation statement"
            )

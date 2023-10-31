module COSE.Decode exposing (decode, decoder)

import Bytes exposing (Bytes)
import Bytes.Decode as Bytes
import COSE exposing (..)
import Cbor.Decode as Cbor exposing (Decoder)


decode : Bytes -> Result (List String) CoseKey
decode =
    Cbor.decode decoder
        >> Maybe.withDefault (Err [ "Failed to decode COSE key" ])



-- bytesDecoder : Bytes.Decoder (Result (List String) CoseKey)


type ReadOperation
    = ReadPropertyCount Int
    | ReadBreak


type State
    = ExpectingPropertyCount Int
    | ReadingProperties Int (List CoseProperty)


bytesDecoder =
    let
        initialState : state
        initialState =
            Debug.todo "initialState"

        loop : state -> Bytes -> Bytes.Step state (Result String state)
        loop state byte =
            case state of
                -- if getting large map size (last iteration), check this byte and combine
                -- with information from previous bytes to get number of properties to read
                -- and store that state
                _ ->
                    case byte of
                        -- TODO:
                        -- if map and is definite size, and size known from just this byte, add
                        -- property reads to stack
                        -- _ ->
                        --   Bytes.Step (updatedState)
                        --
                        -- if map and is definite size and size is more than just this byte, add
                        -- necessary read operations to stack
                        --
                        -- if map and is indefinite size, add break to stack
                        --
                        _ ->
                            Bytes.Done (Err "Unexpected error decoding COSE key")

        finalTransform : state -> Result (List String) CoseKey
        finalTransform =
            Debug.todo "finalTransform"
    in
    Bytes.loop
        (\state ->
            Bytes.bytes 1 |> Bytes.map (loop state)
        )
        initialState


decoder : Decoder (Result (List String) CoseKey)
decoder =
    let
        iterate acc i =
            case i of
                0 ->
                    Cbor.succeed acc

                _ ->
                    cosePropertyDecoder
                        |> Cbor.map (\property -> property :: acc)
                        |> Cbor.andThen (\acc_ -> iterate acc_ (i - 1))

        reduce properties =
            List.foldl
                (\property result ->
                    case ( property, result ) of
                        ( Ok (KeyType keyType), Ok coseKey ) ->
                            Ok { coseKey | keyType = Just keyType }

                        ( Ok (Algorithm algorithm), Ok coseKey ) ->
                            Ok { coseKey | algorithm = Just algorithm }

                        ( Err err, Err errs ) ->
                            Err (err :: errs)

                        ( Err err, Ok _ ) ->
                            Err [ err ]

                        ( _, Err errs ) ->
                            Err errs
                )
                (Ok
                    { keyType = Nothing
                    , algorithm = Nothing

                    -- , keyId = Nothing
                    -- , key = Nothing
                    }
                )
                properties
    in
    Cbor.size
        |> Cbor.andThen (iterate [])
        |> Cbor.thenIgnore Cbor.break
        |> Cbor.map reduce
        |> Cbor.andThen
            (\result ->
                case result of
                    Ok coseKey ->
                        Cbor.succeed
                            (Maybe.map2 CoseKey
                                coseKey.keyType
                                coseKey.algorithm
                                -- coseKey.keyId
                                -- coseKey.key
                                |> Maybe.map Ok
                                |> Maybe.withDefault (Err [ "Failed to decode COSE key" ])
                            )

                    Err errs ->
                        Cbor.succeed (Err errs)
            )


type CoseProperty
    = KeyType KeyType
    | Algorithm Algorithm


cosePropertyDecoder : Decoder (Result String CoseProperty)
cosePropertyDecoder =
    Cbor.int
        |> Cbor.andThen
            (\label ->
                case label of
                    1 ->
                        (Cbor.map << Result.map) KeyType keyTypeDecoder

                    3 ->
                        (Cbor.map << Result.map) Algorithm algorithmDecoder

                    _ ->
                        Cbor.succeed (Err <| "Unexpected COSE label" ++ String.fromInt label)
            )


keyTypeDecoder : Decoder (Result String KeyType)
keyTypeDecoder =
    Cbor.int
        |> Cbor.andThen
            (\label ->
                case label of
                    2 ->
                        Cbor.succeed (Ok EC2)

                    _ ->
                        Cbor.succeed (Err "Unsupported key type")
            )


algorithmDecoder : Decoder (Result String Algorithm)
algorithmDecoder =
    Cbor.int
        |> Cbor.andThen
            (\label ->
                case label of
                    1 ->
                        Cbor.succeed (Ok ES256)

                    2 ->
                        Cbor.succeed (Ok ES384)

                    3 ->
                        Cbor.succeed (Ok ES512)

                    _ ->
                        Cbor.succeed (Err "Unsupported algorithm")
            )

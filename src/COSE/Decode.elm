module COSE.Decode exposing (decoder)

import COSE exposing (..)
import Cbor.Decode as Cbor exposing (Decoder)


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

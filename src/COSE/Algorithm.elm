module COSE.Algorithm exposing (Algorithm(..), cborDecoder, encodeJson, fromInt, jsonDecoder)

import Cbor.Decode
import Json.Decode
import Json.Encode


type Algorithm
    = ES256
    | ES384
    | ES512


jsonDecoder : Json.Decode.Decoder Algorithm
jsonDecoder =
    Json.Decode.int
        |> Json.Decode.andThen
            (fromInt
                >> Maybe.map Json.Decode.succeed
                >> Maybe.withDefault (Json.Decode.fail "Invalid COSE algorithm")
            )


cborDecoder : Cbor.Decode.Decoder Algorithm
cborDecoder =
    Cbor.Decode.int
        |> Cbor.Decode.andThen
            (fromInt
                >> Maybe.map Cbor.Decode.succeed
                >> Maybe.withDefault Cbor.Decode.fail
            )


encodeJson : Algorithm -> Json.Encode.Value
encodeJson alg =
    Json.Encode.int (toInt alg)


toInt : Algorithm -> Int
toInt alg =
    case alg of
        ES256 ->
            -7

        ES384 ->
            -35

        ES512 ->
            -36


fromInt : Int -> Maybe Algorithm
fromInt alg =
    case abs alg of
        7 ->
            Just ES256

        35 ->
            Just ES384

        36 ->
            Just ES512

        _ ->
            Nothing

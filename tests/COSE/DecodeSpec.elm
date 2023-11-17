module COSE.DecodeSpec exposing (suite)

import COSE exposing (..)
import COSE.Algorithm exposing (Algorithm(..))
import COSE.Decode exposing (decoder)
import Cbor exposing (..)
import Cbor.Decode
import Cbor.Encode
import Expect exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "COSE Key Decoder Tests"
        [ test "Decoding a valid COSE key returns the expected result" <|
            \_ ->
                let
                    encoded =
                        { keyType = 3
                        , algorithm = -257
                        }
                            |> Cbor.Encode.record Cbor.Encode.int
                                (Cbor.Encode.fields
                                    >> Cbor.Encode.field 1 Cbor.Encode.int .keyType
                                    >> Cbor.Encode.field 3 Cbor.Encode.int .algorithm
                                )
                            |> Cbor.Encode.encode

                    expectedDecodedKey =
                        CoseKey RSA RS256
                in
                case Cbor.Decode.decode decoder encoded of
                    Just (Ok decoded) ->
                        Expect.equal decoded expectedDecodedKey

                    Just (Err err) ->
                        Expect.fail <| "Decoder failed with error: " ++ err

                    _ ->
                        Expect.fail "Decoder failed"
        ]

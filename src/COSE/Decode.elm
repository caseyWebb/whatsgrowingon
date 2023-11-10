module COSE.Decode exposing (decoder)

import COSE exposing (..)
import COSE.Algorithm as Algorithm exposing (Algorithm)
import Cbor.Decode as Cbor exposing (Decoder)
import Dict
import Result.Extra as Result


decoder : Decoder (Result String CoseKey)
decoder =
    Cbor.dict Cbor.int Cbor.int
        |> Cbor.map
            (Dict.toList
                >> List.map coseProperty
                >> Result.combine
                >> Result.map
                    (List.foldl
                        (\property acc ->
                            case property of
                                KeyType keyType_ ->
                                    { acc | keyType = Just keyType_ }

                                Algorithm algorithm_ ->
                                    { acc | algorithm = Just algorithm_ }
                        )
                        { keyType = Nothing
                        , algorithm = Nothing

                        -- , keyId = Nothing
                        -- , key = Nothing
                        }
                    )
                >> Result.andThen
                    (\key ->
                        case ( key.keyType, key.algorithm ) of
                            ( Just keyType_, Just algorithm_ ) ->
                                Ok <| CoseKey keyType_ algorithm_

                            ( Nothing, _ ) ->
                                Err "COSE key missing kty property"

                            ( _, Nothing ) ->
                                Err "COSE key missing alg property"
                    )
            )


type CoseProperty
    = KeyType KeyType
    | Algorithm Algorithm


coseProperty : ( Int, Int ) -> Result String CoseProperty
coseProperty ( label, value ) =
    let
        toProp ctor toValue propName =
            toValue value
                |> Maybe.map (Ok << ctor)
                |> Maybe.withDefault (Err <| "Unexpected COSE " ++ propName)
    in
    case label of
        1 ->
            toProp KeyType keyType "kty"

        3 ->
            toProp Algorithm Algorithm.fromInt "alg"

        _ ->
            Err <| "Unexpected COSE property"


keyType : Int -> Maybe KeyType
keyType id =
    case id of
        2 ->
            Just EC2

        _ ->
            Nothing

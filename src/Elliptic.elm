module Elliptic exposing (secp256k1, verifySignature)

import BigInt exposing (BigInt, add, div, fromHexString, fromInt, isOdd, mul, sub)
import BigInt.Extra exposing (cube, square)
import Maybe.Extra as Maybe
import SHA256
import Svg.Styled.Attributes exposing (y2)


type Curve
    = Weirstrauss Weirstrauss
    | Montgomery Montgomery


type alias Weirstrauss =
    { a : BigInt
    , b : BigInt
    , p : BigInt
    , g : Point
    , n : BigInt
    }


type alias Montgomery =
    { a : BigInt
    , b : BigInt
    , p : BigInt
    }


type Point
    = Infinity
    | Point BigInt BigInt


secp256k1 : Curve
secp256k1 =
    { a = fromInt 0
    , b = fromInt 7
    , p = BigInt.fromHexString "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"
    , g =
        ( fromHexString "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"
        , fromHexString "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
        )
    , n = BigInt.fromHexString "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"
    }
        |> assertCurve


assertCurve :
    { a : BigInt
    , b : BigInt
    , p : Maybe BigInt
    , g : ( Maybe BigInt, Maybe BigInt )
    , n : Maybe BigInt
    }
    -> Curve
assertCurve ({ a, b } as curve) =
    Maybe.andThen3
        (\p g n ->
            let
                curve =
                    Weirstrauss
                        { a = a
                        , b = b
                        , p = p
                        , g = g
                        , n = n
                        }
            in
            if isOnCurve curve g then
                Just curve

            else
                Nothing
        )
        curve.p
        (Maybe.map2 Point (Tuple.first curve.g) (Tuple.second curve.g))
        curve.n
        |> Maybe.withDefaultLazy (\_ -> Debug.todo "FATAL: Invalid curve")


isOnCurve : Curve -> Point -> Bool
isOnCurve curve point =
    case ( curve, point ) of
        ( _, Infinity ) ->
            True

        ( Weirstrauss { a, b, p }, Point x y ) ->
            Just (square y) == BigInt.modBy p (add (cube x) (add (mul a x) b))

        ( Montgomery { a, p }, Point x y ) ->
            Just (square y) == BigInt.modBy p (add (mul (square x) x) (add (mul a (square x)) x))


addPoints : Curve -> Point -> Point -> Maybe Point
addPoints curve p1 p2 =
    let
        generalCase { p, x1, y1, x2, y2 } =
            let
                m =
                    div (sub y2 y1) (sub x2 x1)

                x3_ =
                    BigInt.modBy p (sub (sub (square m) x1) x2)

                y3_ =
                    x3_ |> Maybe.andThen (\x3 -> BigInt.modBy p (sub (mul m (sub x1 x3)) y1))
            in
            Maybe.map2 Point x3_ y3_
    in
    case ( curve, p1, p2 ) of
        ( _, Infinity, _ ) ->
            Just p2

        ( _, _, Infinity ) ->
            Just p1

        ( Weirstrauss { a, p }, Point x1 y1, Point x2 y2 ) ->
            if x1 == x2 then
                if y1 == BigInt.negate y2 || y1 == fromInt 0 then
                    -- If the points are vertically opposite (add to zero), the result is the point at infinity
                    Just Infinity

                else
                    -- Point doubling case
                    let
                        m =
                            div (add (mul (fromInt 3) (square x1)) a) (mul (fromInt 2) y1)

                        x3_ =
                            BigInt.modBy p (sub (sub (square m) x1) x2)

                        y3_ =
                            x3_ |> Maybe.andThen (\x3 -> BigInt.modBy p (sub (mul m (sub x1 x3)) y1))
                    in
                    Maybe.map2 Point x3_ y3_

            else
                generalCase { p = p, x1 = x1, y1 = y1, x2 = x2, y2 = y2 }

        ( Montgomery { a, b, p }, Point x1 y1, Point x2 y2 ) ->
            if x1 == x2 then
                if y1 == y2 then
                    -- Point doubling
                    if y1 == fromInt 0 then
                        -- Doubling a point where y1 is zero results in the point at infinity
                        Just Infinity

                    else
                        let
                            numerator =
                                add (add (mul (fromInt 3) (square x1)) (mul (fromInt 2) (mul a x1))) (fromInt 1)

                            denominator =
                                mul (fromInt 2) (mul b (square y1))

                            x3_ =
                                BigInt.modBy p (sub (div (square numerator) denominator) (mul (fromInt 2) x1))

                            y3_ =
                                x3_ |> Maybe.andThen (\x3 -> BigInt.modBy p (div (sub (mul numerator (sub x1 x3)) (mul (fromInt 2) (square y1))) (mul (fromInt 2) y1)))
                        in
                        Maybe.map2 Point x3_ y3_

                else
                    -- Adding inverse points, result is infinity
                    Just Infinity

            else
                generalCase { p = p, x1 = x1, y1 = y1, x2 = x2, y2 = y2 }


doublePoint : Curve -> Point -> Maybe Point
doublePoint curve point =
    addPoints curve point point


scalarMultiplication : Curve -> BigInt -> Point -> Maybe Point
scalarMultiplication curve scalar point =
    let
        doubleAndAdd k =
            Maybe.andThen2
                (\currentPoint resultPoint ->
                    if k == fromInt 0 then
                        Just resultPoint

                    else
                        let
                            newResultPoint =
                                if isOdd k then
                                    addPoints curve resultPoint currentPoint

                                else
                                    Just resultPoint

                            newCurrentPoint =
                                doublePoint curve currentPoint
                        in
                        doubleAndAdd (div k (fromInt 2)) newCurrentPoint newResultPoint
                )
    in
    doubleAndAdd scalar (Just point) (Just Infinity)


verifySignature : Curve -> String -> Point -> Point -> Bool
verifySignature curve message sig publicKey =
    let
        { g, n } =
            case curve of
                Weirstrauss c ->
                    c

                Montgomery c ->
                    c
    in
    case sig of
        Infinity ->
            False

        Point r s ->
            if s == fromInt 0 then
                False

            else
                BigInt.fromHexString (SHA256.toHex (SHA256.fromString message))
                    |> Maybe.andThen
                        (\e ->
                            Maybe.andThen2
                                (\u1 u2 ->
                                    Maybe.andThen2 (addPoints curve)
                                        (scalarMultiplication curve u1 g)
                                        (scalarMultiplication curve u2 publicKey)
                                )
                                (BigInt.modBy n <| div e s)
                                (BigInt.modBy n <| div r s)
                        )
                    |> Maybe.map
                        (\point ->
                            case point of
                                Infinity ->
                                    False

                                Point r_ _ ->
                                    r == r_
                        )
                    |> Maybe.withDefault False

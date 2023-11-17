module BigInt.Extra exposing (..)

import BigInt exposing (..)


square : BigInt -> BigInt
square x =
    pow x (fromInt 2)


cube : BigInt -> BigInt
cube x =
    pow x (fromInt 3)

module Evergreen.V1.RemoteData exposing (..)


type RemoteData err a
    = NotAsked
    | Loading
    | Failure err
    | Success a

module Data.Users exposing (Model, Passkey, User, UserId, get, init, insert, toString)

import Array exposing (Array)
import Dict exposing (Dict)


type UserId
    = UserId Int


type alias User =
    { id : UserId
    , passkey : Passkey
    }


type alias Passkey =
    { counter : Int
    , credentialId : String
    , publicKey : String
    }


type alias Model =
    { users : Dict Int User
    , counter : Int
    }


init : Model
init =
    { users = Dict.empty
    , counter = 0
    }


toString : UserId -> String
toString (UserId id) =
    String.fromInt id


insert : Model -> (UserId -> User) -> ( User, Model )
insert model user =
    let
        newUser =
            user (UserId model.counter)
    in
    ( newUser
    , { users = Dict.insert model.counter newUser model.users
      , counter = model.counter + 1
      }
    )


get : Model -> UserId -> Maybe User
get model (UserId id) =
    Dict.get id model.users

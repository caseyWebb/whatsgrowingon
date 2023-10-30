module Data.Users exposing
    ( Model
    , Passkey
    , User
    , UserId
    , Username
    , findByPasskeyId
    , findByUsername
    , get
    , idToString
    , init
    , insert
    , passkeyDecoder
    , passkeyEncoder
    , userIdDecoder
    , usernameDecoder
    , usernameToString
    )

import GenericDict as Dict exposing (Dict)
import Html exposing (u)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Svg.Styled exposing (use)


type UserId
    = UserId Int


userIdDecoder : Decoder UserId
userIdDecoder =
    Decode.map UserId Decode.int


type Username
    = Username String


usernameDecoder : Decoder Username
usernameDecoder =
    Decode.map Username Decode.string


type Passkey
    = Passkey
        { id : String
        , counter : Int
        , publicKey : String
        }


passkeyDecoder : Decoder Passkey
passkeyDecoder =
    Decode.map3
        (\id counter publicKey ->
            Passkey
                { id = id
                , counter = counter
                , publicKey = publicKey
                }
        )
        (Decode.field "credentialID" Decode.string)
        (Decode.field "counter" Decode.int)
        (Decode.field "publicKey" Decode.string)


passkeyEncoder : Passkey -> Value
passkeyEncoder (Passkey passkey) =
    Encode.object
        [ ( "credentialID", Encode.string passkey.id )
        , ( "counter", Encode.int passkey.counter )
        , ( "publicKey", Encode.string passkey.publicKey )
        ]


type alias User =
    { id : UserId
    , username : Username
    , passkey : Passkey
    }


type Model
    = Model
        { users : Dict UserId User
        , usersByUsernameIndex : Dict Username UserId
        , usersByPasskeyIndex : Dict String UserId
        , counter : Int
        }


init : Model
init =
    Model
        { users = Dict.empty
        , usersByUsernameIndex = Dict.empty
        , usersByPasskeyIndex = Dict.empty
        , counter = 0
        }


idToString : UserId -> String
idToString (UserId id) =
    String.fromInt id


usernameToString : Username -> String
usernameToString (Username username) =
    username


insert :
    Model
    ->
        { username : Username
        , passkey : Passkey
        }
    -> ( UserId, Model )
insert (Model model) { passkey, username } =
    let
        userId =
            UserId model.counter

        passkeyId =
            case passkey of
                Passkey { id } ->
                    Debug.log "passkeyId" id
    in
    ( userId
    , Model
        { users =
            Dict.insert (\(UserId id) -> String.fromInt id)
                userId
                { id = userId
                , passkey = passkey
                , username = username
                }
                model.users
        , usersByUsernameIndex =
            Dict.insert (\(Username str) -> str) username userId model.usersByUsernameIndex
        , usersByPasskeyIndex =
            Dict.insert identity passkeyId userId model.usersByPasskeyIndex
        , counter = model.counter + 1
        }
    )


get : Model -> UserId -> Maybe User
get (Model model) id =
    Dict.get (\(UserId i) -> String.fromInt i) id model.users


findByUsername : Model -> String -> Maybe User
findByUsername (Model model) username =
    Dict.get (\(Username str) -> str) (Username username) model.usersByUsernameIndex
        |> Maybe.andThen (get (Model model))


findByPasskeyId : Model -> String -> Maybe User
findByPasskeyId (Model model) passkeyId =
    Dict.get identity passkeyId model.usersByPasskeyIndex
        |> Maybe.andThen (get (Model model))

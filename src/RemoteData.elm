module RemoteData exposing (..)


type RemoteData err a
    = NotAsked
    | Loading
    | Failure err
    | Success a


andThen : (a -> RemoteData err b) -> RemoteData err a -> RemoteData err b
andThen f remoteData =
    case remoteData of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Failure err ->
            Failure err

        Success a ->
            f a


map : (a -> b) -> RemoteData err a -> RemoteData err b
map f =
    andThen (Success << f)

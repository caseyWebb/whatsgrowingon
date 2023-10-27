module Effect exposing
    ( Effect, none, map, batch
    , focus
    , fromCmd, fromShared
    , toCmd
    )

{-|

@docs Effect, none, map, batch
@docs focus
@docs fromCmd, fromShared
@docs toCmd

-}

import Browser.Dom as Dom
import Shared
import Task


type Effect msg
    = None
    | Cmd (Cmd msg)
    | Shared (Shared.Msg msg)
    | Batch (List (Effect msg))
    | Focus String (Result Dom.Error () -> msg)


none : Effect msg
none =
    None


map : (a -> b) -> Effect a -> Effect b
map fn effect =
    case effect of
        None ->
            None

        Cmd cmd ->
            Cmd (Cmd.map fn cmd)

        Shared msg ->
            Shared (Shared.mapMsg fn msg)

        Batch list ->
            Batch (List.map (map fn) list)

        Focus str onFocusResult ->
            Focus str (onFocusResult >> fn)


fromCmd : Cmd msg -> Effect msg
fromCmd =
    Cmd


fromShared : Shared.Msg msg -> Effect msg
fromShared =
    Shared


batch : List (Effect msg) -> Effect msg
batch =
    Batch


focus : String -> (Result Dom.Error () -> msg) -> Effect msg
focus =
    Focus



-- Used by Main.elm


toCmd : ( Shared.Msg pageMsg -> msg, pageMsg -> msg ) -> Effect pageMsg -> Cmd msg
toCmd ( fromSharedMsg, fromPageMsg ) effect =
    case effect of
        None ->
            Cmd.none

        Cmd cmd ->
            Cmd.map fromPageMsg cmd

        Shared msg ->
            Task.succeed msg
                |> Task.perform fromSharedMsg

        Batch list ->
            Cmd.batch (List.map (toCmd ( fromSharedMsg, fromPageMsg )) list)

        Focus str onFocusResult ->
            Dom.focus str
                |> Task.attempt (onFocusResult >> fromPageMsg)

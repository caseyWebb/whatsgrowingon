module Effect exposing
    ( Effect, none, map, batch
    , focus, getCurrentTime
    , fromCmd, fromShared
    , toCmd
    )

{-|

@docs Effect, none, map, batch
@docs focus, getCurrentTime
@docs fromCmd, fromShared
@docs toCmd

-}

import Browser.Dom as Dom
import Shared
import Task
import Time


type Effect msg
    = None
    | Cmd (Cmd msg)
    | Shared Shared.Msg
    | Batch (List (Effect msg))
    | Focus String (Result Dom.Error () -> msg)
    | GetCurrentTime (Time.Posix -> msg)


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
            Shared msg

        Batch list ->
            Batch (List.map (map fn) list)

        Focus str onFocusResult ->
            Focus str (onFocusResult >> fn)

        GetCurrentTime gotTime ->
            GetCurrentTime (gotTime >> fn)


fromCmd : Cmd msg -> Effect msg
fromCmd =
    Cmd


fromShared : Shared.Msg -> Effect msg
fromShared =
    Shared


batch : List (Effect msg) -> Effect msg
batch =
    Batch


focus : String -> (Result Dom.Error () -> msg) -> Effect msg
focus =
    Focus


getCurrentTime : (Time.Posix -> msg) -> Effect msg
getCurrentTime =
    GetCurrentTime



-- Used by Main.elm


toCmd :
    ( Shared.Msg -> msg
    , pageMsg -> msg
    )
    -> Effect pageMsg
    -> Cmd msg
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

        GetCurrentTime gotTime ->
            Time.now |> Task.perform (gotTime >> fromPageMsg)

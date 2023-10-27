module Evergreen.V1.Ui.Modal exposing (..)


type Msg
    = Close
    | NoOp


type Model modalKind msg
    = Opened modalKind msg
    | Closed

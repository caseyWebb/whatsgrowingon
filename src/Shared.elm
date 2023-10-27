module Shared exposing
    ( Model
    , Msg
    , ToBackend(..)
    , ToFrontend(..)
    , addZone
    , deleteZone
    , fromBackend
    , init
    , subscriptions
    , update
    , updateZone
    , view
    )

import Browser.Dom as Dom
import Css
import Css.Media
import Data exposing (..)
import GenericDict as Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Maybe.Extra as Maybe
import Random
import RemoteData exposing (RemoteData)
import Request exposing (Request)
import Slug exposing (Slug)
import Task
import Time
import View exposing (View)


addZone : Msg
addZone =
    AddZone Nothing


updateZone : Bool -> Zone -> Msg
updateZone =
    UpdateZone


fromBackend : ToFrontend -> Msg
fromBackend =
    FromBackend


deleteZone : Slug -> Msg
deleteZone =
    DeleteZone



-- INIT


type alias Model =
    { data :
        RemoteData
            String
            { zones : Dict Slug Zone
            , crops : Dict Slug Crop
            , varieties : Dict Slug Variety
            }
    , now : Maybe Time.Posix
    }


init : { toBackend : ToBackend -> Cmd Msg } -> Request -> ( Model, Cmd Msg )
init { toBackend } _ =
    ( { data = RemoteData.Loading
      , now = Nothing
      }
    , Cmd.batch
        [ toBackend <| FetchData
        , Time.now |> Task.perform GotCurrentTime
        ]
    )



-- UPDATE


type Msg
    = FromBackend ToFrontend
    | AddZone (Maybe Slug)
    | DeleteZone Slug
    | FocusZone Slug
    | UpdateZone Bool Zone
    | GotCurrentTime Time.Posix
    | NoOp


type ToBackend
    = FetchData
    | SaveZone Zone
    | DeleteZoneToBackend Slug


type ToFrontend
    = GotData
        { zones : Dict Slug Zone
        , crops : Dict Slug Crop
        , varieties : Dict Slug Variety
        }


update :
    { toBackend : ToBackend -> Cmd Msg
    }
    -> Request
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update ({ toBackend } as config) req msg model =
    let
        andThen msg_ ( updatedModel, cmd ) =
            update config req msg_ updatedModel |> Tuple.mapSecond (List.singleton >> (::) cmd >> Cmd.batch)
    in
    case msg of
        FromBackend (GotData data) ->
            ( { model
                | data = RemoteData.Success data
              }
            , Cmd.none
            )

        AddZone maybeSlug ->
            case maybeSlug of
                Nothing ->
                    ( model
                    , Random.generate (AddZone << Just) Slug.random
                    )

                Just slug ->
                    let
                        index =
                            RemoteData.map (.zones >> Dict.size) model.data
                                |> RemoteData.toMaybe
                                |> Maybe.withDefault 0

                        newZone =
                            Zone slug index "" []
                    in
                    update config req (UpdateZone True newZone) model
                        |> andThen (FocusZone slug)

        FocusZone slug ->
            ( model, Slug.map Dom.focus slug |> Task.attempt (always NoOp) )

        UpdateZone save zone ->
            ( { model
                | data =
                    RemoteData.map
                        (\data ->
                            { data
                                | zones = Dict.insert (Slug.map identity) zone.slug zone data.zones
                            }
                        )
                        model.data
              }
            , if save then
                toBackend (SaveZone zone)

              else
                Cmd.none
            )

        DeleteZone zone ->
            ( { model
                | data =
                    RemoteData.map
                        (\data ->
                            { data
                                | zones = Dict.remove (Slug.map identity) zone data.zones
                            }
                        )
                        model.data
              }
            , toBackend (DeleteZoneToBackend zone)
            )

        GotCurrentTime maybePosix ->
            ( { model | now = Just maybePosix }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none



-- VIEW


view :
    Request
    ->
        { page : View msg
        , toMsg : Msg -> msg
        }
    -> View msg
view _ { page } =
    { title = page.title
    , body =
        [ div
            [ css
                [ Css.fontFamily Css.sansSerif
                , Css.width (Css.pct 100)
                , Css.minHeight (Css.vh 100)
                , Css.overflow Css.auto
                , Css.padding (Css.em 1)
                , Css.boxSizing Css.borderBox
                , Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                    [ Css.backgroundColor (Css.hex "030303")
                    , Css.color (Css.hex "ccc")
                    ]
                ]
            ]
            page.body
        ]
    , modal = page.modal
    }

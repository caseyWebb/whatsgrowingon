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
import Browser.Events exposing (onKeyDown)
import Css
import Css.Media
import Data exposing (..)
import GenericDict as Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attrs exposing (attribute, css)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Decode as Decode
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



-- showAddPlantingModal : pageMsg -> Zone -> Msg pageMsg
-- showAddPlantingModal returnFocusTo zone =
--     ShowModal returnFocusTo (AddPlantingModal (AddPlantingModalStep1 zone))


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


type AddPlantingStep
    = AddPlantingModalStep1 Zone
    | AddPlantingModalStep2 Zone Crop
    | AddPlantingModalStep3 Zone Crop Variety Int


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
    | CloseModal
    | AddZone (Maybe Slug)
    | DeleteZone Slug
    | FocusZone Slug
    | UpdateZone Bool Zone
    | AdvanceAddPlantingModal AddPlantingStep
    | OnNewPlantingAmountChange String
    | AddPlanting Slug Slug Slug Int (Maybe Time.Posix)
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
            update config req msg_ updatedModel
                |> Tuple.mapSecond
                    (\cmd_ -> Cmd.batch [ cmd, cmd_ ])
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

        -- ShowModal returnFocusTo modal ->
        --     ( { model | modal = Just (Modal modal) }
        --     , Cmd.none
        --     )
        --         |> Tuple.mapSecond (Cmd.map config.wrapSharedMsg)
        -- CloseModal ->
        --     ( { model | modal = Nothing }
        --     , Cmd.none
        --     )
        -- AdvanceAddPlantingModal step ->
        --     ( { model
        --        | modal =
        --             Maybe.map
        --                 (\(Modal returnFocusTo _) -> Modal returnFocusTo <| AddPlantingModal step)
        --                 model.modal
        --       }
        --     , Cmd.none
        --     )
        -- OnNewPlantingAmountChange amountStr ->
        --     case ( model.modal, String.toInt amountStr ) of
        --         ( Just (Modal returnFocusTo (AddPlantingModal (AddPlantingModalStep3 zoneSlug crop variety _))), Just newAmount ) ->
        --             ( { model
        --                 | modal =
        --                     Just
        --                         (Modal returnFocusTo
        --                             (AddPlantingModal (AddPlantingModalStep3 zoneSlug crop variety newAmount))
        --                         )
        --               }
        --             , Cmd.none
        --             )
        --         _ ->
        --             ( model, Cmd.none )
        -- AddPlanting zoneSlug cropSlug varietySlug amount maybePosix ->
        --     case Maybe.or maybePosix model.now of
        --         Nothing ->
        --             ( model
        --             , Time.now
        --                 |> Task.perform
        --                     (Just >> AddPlanting zoneSlug cropSlug varietySlug amount)
        --             )
        --                 |> Tuple.mapSecond (Cmd.map config.wrapSharedMsg)
        --         Just now ->
        --             let
        --                 newPlanting =
        --                     Planting cropSlug varietySlug amount now
        --             in
        --             model.data
        --                 |> RemoteData.map (.zones >> Dict.get (Slug.map identity) zoneSlug)
        --                 |> RemoteData.toMaybe
        --                 |> Maybe.andThen identity
        --                 |> Maybe.map
        --                     (\zone ->
        --                         update config
        --                             req
        --                             (UpdateZone True
        --                                 { zone | plantings = newPlanting :: zone.plantings }
        --                             )
        --                             model
        --                     )
        --                 |> Maybe.withDefault ( model, Cmd.none )
        --                 |> andThen CloseModal
        GotCurrentTime maybePosix ->
            ( { model | now = Just maybePosix }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none



-- VIEW


addPlantingModalCropButtonId : Int -> String
addPlantingModalCropButtonId index =
    "add-planting-modal-crop-button-" ++ String.fromInt index


addPlantingModalVarietyButtonId : Int -> String
addPlantingModalVarietyButtonId index =
    "add-planting-modal-variety-button-" ++ String.fromInt index


addPlantingModalAmountInputId : String
addPlantingModalAmountInputId =
    "add-planting-modal-amount-input"


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

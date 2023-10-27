module Shared exposing
    ( Modal(..)
    , Model
    , Msg
    , ToBackend(..)
    , ToFrontend(..)
    , addZone
    , fromBackend
    , init
    , showAddPlantingModal
    , showConfirmDeleteZoneModal
    , subscriptions
    , update
    , updateZone
    , view
    )

import Css
import Css.Media
import Data exposing (..)
import GenericDict as Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attrs exposing (attribute, css)
import Html.Styled.Events exposing (onClick, onInput)
import Maybe.Extra as Maybe
import Random
import RemoteData exposing (RemoteData)
import Request exposing (Request)
import Slug exposing (Slug)
import Task
import Time
import Ui.Button as Button
import Ui.Color as Color
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


showAddPlantingModal : Zone -> Msg
showAddPlantingModal zone =
    ShowModal (AddPlantingModal <| AddPlantingModalStep1 zone)


showConfirmDeleteZoneModal : Zone -> Msg
showConfirmDeleteZoneModal zone =
    ShowModal (ConfirmDeleteZoneModal zone)



-- INIT


type alias Model =
    { data :
        RemoteData
            String
            { zones : Dict Slug Zone
            , crops : Dict Slug Crop
            , varieties : Dict Slug Variety
            }
    , modal : Maybe Modal
    , now : Maybe Time.Posix
    }


type Modal
    = AddPlantingModal AddPlantingStep
    | ConfirmDeleteZoneModal Zone


type AddPlantingStep
    = AddPlantingModalStep1 Zone
    | AddPlantingModalStep2 Zone Crop
    | AddPlantingModalStep3 Zone Crop Variety Int


init : { toBackend : ToBackend -> Cmd Msg } -> Request -> ( Model, Cmd Msg )
init { toBackend } _ =
    ( { data = RemoteData.Loading
      , modal = Nothing
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
    | UpdateZone Bool Zone
    | ConfirmDeleteZone Zone
    | ShowModal Modal
    | CloseModal
    | AdvanceAddPlantingModal AddPlantingStep
    | OnNewPlantingAmountChange String
    | AddPlanting Slug Slug Slug Int (Maybe Time.Posix)
    | GotCurrentTime Time.Posix


type ToBackend
    = FetchData
    | SaveZone Zone
    | DeleteZone Slug


type ToFrontend
    = GotData
        { zones : Dict Slug Zone
        , crops : Dict Slug Crop
        , varieties : Dict Slug Variety
        }


update : { toBackend : ToBackend -> Cmd Msg } -> Request -> Msg -> Model -> ( Model, Cmd Msg )
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

                        zone =
                            Slug.map (\str -> Zone slug index str []) slug
                    in
                    update config req (UpdateZone True zone) model

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

        ConfirmDeleteZone zone ->
            ( { model
                | data =
                    RemoteData.map
                        (\data ->
                            { data
                                | zones = Dict.remove (Slug.map identity) zone.slug data.zones
                            }
                        )
                        model.data
              }
            , toBackend (DeleteZone zone.slug)
            )
                |> andThen CloseModal

        ShowModal modal ->
            ( { model | modal = Just modal }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modal = Nothing }
            , Cmd.none
            )

        AdvanceAddPlantingModal step ->
            ( { model | modal = Just (AddPlantingModal step) }
            , Cmd.none
            )

        OnNewPlantingAmountChange amountStr ->
            case ( model.modal, String.toInt amountStr ) of
                ( Just (AddPlantingModal (AddPlantingModalStep3 zoneSlug crop variety _)), Just newAmount ) ->
                    ( { model
                        | modal = Just (AddPlantingModal <| AddPlantingModalStep3 zoneSlug crop variety newAmount)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AddPlanting zoneSlug cropSlug varietySlug amount maybePosix ->
            case Maybe.or maybePosix model.now of
                Nothing ->
                    ( model, Time.now |> Task.perform (Just >> AddPlanting zoneSlug cropSlug varietySlug amount) )

                Just now ->
                    let
                        planting =
                            Planting cropSlug varietySlug amount now []
                    in
                    model.data
                        |> RemoteData.map (.zones >> Dict.get (Slug.map identity) zoneSlug)
                        |> RemoteData.toMaybe
                        |> Maybe.andThen identity
                        |> Maybe.map (\zone -> update config req (UpdateZone True { zone | plantings = planting :: zone.plantings }) model)
                        |> Maybe.withDefault ( model, Cmd.none )
                        |> andThen CloseModal

        GotCurrentTime maybePosix ->
            ( { model | now = Just maybePosix }
            , Cmd.none
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none



-- VIEW


view :
    Request
    -> { page : View msg, toMsg : Msg -> msg }
    -> Model
    -> View msg
view _ { page, toMsg } model =
    { title = page.title
    , body =
        [ (case model.modal of
            Just modal_ ->
                [ div [ attribute "inert" "true" ] page.body
                , viewModal model modal_ |> Html.map toMsg
                ]

            Nothing ->
                page.body
          )
            |> div
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
        ]
    }


viewModal : Model -> Modal -> Html Msg
viewModal model modal =
    Html.div
        [ css
            [ Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            , Css.position Css.fixed
            , Css.top Css.zero
            , Css.right Css.zero
            , Css.bottom Css.zero
            , Css.left Css.zero
            ]
        ]
        [ viewModalBackdrop
        , viewModalContent <|
            case ( modal, model.data ) of
                ( AddPlantingModal modalModel, RemoteData.Success data ) ->
                    viewAddPlantingModal data modalModel

                ( ConfirmDeleteZoneModal zone, _ ) ->
                    viewConfirmDeleteZoneModal zone

                ( _, RemoteData.Loading ) ->
                    Html.text "Loading..."

                ( _, RemoteData.Failure _ ) ->
                    Html.text "Something went wrong..."

                ( _, RemoteData.NotAsked ) ->
                    Html.text "Something when wrong..."
        ]


viewModalBackdrop : Html Msg
viewModalBackdrop =
    div
        [ css
            [ Css.position Css.fixed
            , Css.top (Css.px 0)
            , Css.left (Css.px 0)
            , Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            , Css.backgroundColor (Css.rgba 255 255 255 0.8)
            , Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                [ Css.backgroundColor (Css.rgba 0 0 0 0.95)
                ]
            ]
        , onClick CloseModal
        ]
        []


viewModelCloseButton : Html Msg
viewModelCloseButton =
    Html.button
        [ css
            [ Css.position Css.absolute
            , Css.top (Css.px -30)
            , Css.right (Css.px -30)
            , Css.width (Css.px 30)
            , Css.height (Css.px 30)
            , Css.fontWeight Css.bold
            , Css.fontSize (Css.px 30)
            , Css.border Css.zero
            , Css.outline Css.none
            , Css.backgroundColor (Css.hex "fff")
            , Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                [ Css.color (Css.hex "fff")
                , Css.backgroundColor (Css.hex "000")
                ]
            ]
        , onClick CloseModal
        ]
        [ text "X" ]


viewModalContent : Html Msg -> Html Msg
viewModalContent content =
    Html.div
        [ css
            [ Css.borderRadius (Css.px 6)
            , Css.width (Css.px 1200)
            , Css.maxWidth (Css.vw 90)
            , Css.position Css.relative
            ]
        ]
        [ viewModelCloseButton
        , Html.div
            [ css
                [ Css.overflowX Css.hidden
                , Css.overflowY Css.auto
                , Css.maxHeight (Css.vh 100)
                ]
            ]
            [ content ]
        ]


viewAddPlantingModal : { data | crops : Dict Slug Crop, varieties : Dict Slug Variety } -> AddPlantingStep -> Html Msg
viewAddPlantingModal { crops, varieties } modalModel =
    let
        largeButtonStyles =
            Css.important <|
                Css.batch
                    [ Css.padding2 (Css.em 2) (Css.em 5)
                    , Css.margin2 (Css.px 10) (Css.em 0.5)
                    ]

        wrappingFlexRow =
            Html.div
                [ css
                    [ Css.displayFlex
                    , Css.flexWrap Css.wrap
                    , Css.justifyContent Css.center
                    ]
                ]
    in
    case modalModel of
        AddPlantingModalStep1 zoneSlug ->
            Dict.values crops
                |> List.map
                    (\crop ->
                        Button.view (AdvanceAddPlantingModal (AddPlantingModalStep2 zoneSlug crop))
                            [ css
                                [ Color.styles crop.color
                                , largeButtonStyles
                                ]
                            ]
                            [ Html.text crop.name ]
                    )
                |> wrappingFlexRow

        AddPlantingModalStep2 zoneSlug selectedCrop ->
            List.filterMap (\slug -> Dict.get (Slug.map identity) slug varieties) selectedCrop.varieties
                |> List.map
                    (\variety ->
                        Button.view (AdvanceAddPlantingModal (AddPlantingModalStep3 zoneSlug selectedCrop variety 100))
                            [ css
                                [ Color.styles (Maybe.withDefault selectedCrop.color variety.color)
                                , largeButtonStyles
                                ]
                            ]
                            [ Html.text variety.name ]
                    )
                |> wrappingFlexRow

        AddPlantingModalStep3 zone selectedCrop selectedVariety desiredAmount ->
            let
                capacity =
                    100 - List.sum (List.map .amount zone.plantings)

                amount =
                    min desiredAmount capacity
            in
            Html.div [ css [ Css.displayFlex, Css.flexDirection Css.column, Css.alignItems Css.center, Css.padding (Css.em 1) ] ]
                [ Html.input
                    [ Attrs.type_ "range"
                    , Attrs.min "0"
                    , Attrs.max (String.fromInt capacity)
                    , Attrs.step "5"
                    , Attrs.value (String.fromInt amount)
                    , onInput OnNewPlantingAmountChange
                    , css
                        [ Css.width (Css.pct 100)
                        ]
                    ]
                    []
                , Html.p [] [ Html.text <| String.fromInt amount ++ "%" ]
                , Button.view (AddPlanting zone.slug selectedCrop.slug selectedVariety.slug amount Nothing) [] [ Html.text "Add" ]
                ]


viewConfirmDeleteZoneModal : Zone -> Html Msg
viewConfirmDeleteZoneModal zone =
    Html.div
        [ css
            [ Css.padding (Css.em 1)
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            ]
        ]
        [ Html.p [] [ Html.text <| "Are you sure you want to delete " ++ zone.name ++ "?" ]
        , Html.div
            [ css
                [ Css.displayFlex
                , Css.property "gap" "1em"
                ]
            ]
            [ Button.view (ConfirmDeleteZone zone) [ css [ Color.styles Color.Red ] ] [ Html.text "Delete" ]
            , Button.view CloseModal [] [ Html.text "Cancel" ]
            ]
        ]

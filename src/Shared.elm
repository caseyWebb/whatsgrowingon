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


showAddPlantingModal : Slug -> Msg
showAddPlantingModal zoneSlug =
    ShowModal (AddPlantingModal <| AddPlantingModalStep1 zoneSlug)



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


type AddPlantingStep
    = AddPlantingModalStep1 Slug
    | AddPlantingModalStep2 Slug Slug
    | AddPlantingModalStep3 Slug Slug Slug Int


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
    | ShowModal Modal
    | CloseModal
    | AdvanceAddPlantingModal AddPlantingStep
    | OnNewPlantingAmountChange String
    | AddPlanting Slug Slug Slug Int (Maybe Time.Posix)
    | GotCurrentTime Time.Posix


type ToBackend
    = FetchData
    | SaveZone Zone


type ToFrontend
    = GotData
        { zones : Dict Slug Zone
        , crops : Dict Slug Crop
        , varieties : Dict Slug Variety
        }


update : { toBackend : ToBackend -> Cmd Msg } -> Request -> Msg -> Model -> ( Model, Cmd Msg )
update ({ toBackend } as config) req msg model =
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
                            Planting cropSlug varietySlug (toFloat amount / 100) now []

                        ( updatedModel, updateZoneCmd ) =
                            model.data
                                |> RemoteData.map (.zones >> Dict.get (Slug.map identity) zoneSlug)
                                |> RemoteData.toMaybe
                                |> Maybe.andThen identity
                                |> Maybe.map (\zone -> update config req (UpdateZone True { zone | plantings = planting :: zone.plantings }) model)
                                |> Maybe.withDefault ( model, Cmd.none )
                    in
                    update config req CloseModal updatedModel
                        |> Tuple.mapSecond
                            (\closedModalCmd ->
                                Cmd.batch
                                    [ closedModalCmd
                                    , updateZoneCmd
                                    ]
                            )

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

                ( _, _ ) ->
                    Html.text "Loading..."
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
                [ Css.backgroundColor (Css.rgba 0 0 0 0.8)
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
            , Css.borderRadius (Css.pct 50)
            , Css.outline Css.none
            , Css.fontWeight Css.bold
            , Css.fontSize (Css.px 20)
            , Css.border3 (Css.px 3) Css.solid (Css.hex "111")
            , Css.backgroundColor (Css.hex "fff")
            , Css.hover
                [ Css.backgroundColor (Css.hex "111")
                , Css.color (Css.hex "fff")
                ]
            , Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                [ Css.color (Css.hex "fff")
                , Css.backgroundColor (Css.rgba 0 0 0 1)
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
            , Css.width (Css.px 600)
            , Css.maxWidth (Css.vw 90)
            , Css.position Css.relative
            , Css.padding2 Css.zero (Css.px 20)
            ]
        ]
        [ viewModelCloseButton
        , content
        ]


viewAddPlantingModal : { data | crops : Dict Slug Crop, varieties : Dict Slug Variety } -> AddPlantingStep -> Html Msg
viewAddPlantingModal { crops, varieties } modalModel =
    let
        buttonStyles =
            css
                [ Css.padding (Css.em 1)
                , Css.margin2 (Css.px 10) (Css.em 0.5)
                , Css.fontWeight Css.bold
                , Css.textTransform Css.uppercase
                , Css.borderRadius (Css.px 6)
                , Css.outline Css.none
                , Css.border3 (Css.px 3) Css.solid (Css.hex "111")
                , Css.backgroundColor (Css.hex "fff")
                , Css.hover
                    [ Css.backgroundColor (Css.hex "111")
                    , Css.color (Css.hex "fff")
                    ]
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
                        Html.button
                            [ buttonStyles
                            , onClick (AdvanceAddPlantingModal (AddPlantingModalStep2 zoneSlug crop.slug))
                            ]
                            [ Html.text crop.name ]
                    )
                |> wrappingFlexRow

        AddPlantingModalStep2 zoneSlug selectedCrop ->
            Dict.get (Slug.map identity) selectedCrop crops
                |> Maybe.map .varieties
                |> Maybe.withDefault []
                |> List.filterMap (\slug -> Dict.get (Slug.map identity) slug varieties)
                |> List.map
                    (\variety ->
                        Html.button
                            [ buttonStyles
                            , onClick (AdvanceAddPlantingModal (AddPlantingModalStep3 zoneSlug selectedCrop variety.slug 100))
                            ]
                            [ Html.text variety.name ]
                    )
                |> wrappingFlexRow

        AddPlantingModalStep3 zoneSlug selectedCrop selectedVariety amount ->
            Html.div [ css [ Css.displayFlex, Css.flexDirection Css.column, Css.alignItems Css.center ] ]
                [ Html.input
                    [ Attrs.type_ "range"
                    , Attrs.min "0"
                    , Attrs.max "100"
                    , Attrs.step "5"
                    , Attrs.value (String.fromInt amount)
                    , onInput OnNewPlantingAmountChange
                    , css
                        [ Css.width (Css.pct 100)
                        ]
                    ]
                    []
                , Html.p [] [ Html.text <| String.fromInt amount ++ "%" ]
                , Html.button [ buttonStyles, onClick (AddPlanting zoneSlug selectedCrop selectedVariety amount Nothing) ]
                    [ Html.text "Add" ]
                ]

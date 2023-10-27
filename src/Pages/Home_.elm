module Pages.Home_ exposing (Model, Msg, page)

import Css
import Css.Media as Media
import Data exposing (..)
import Effect exposing (Effect)
import GenericDict as Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div, p, span)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Maybe.Extra as Maybe
import Page
import RemoteData exposing (RemoteData(..))
import Request exposing (Request)
import Shared
import Slug exposing (Slug)
import Time
import Ui.Button as Button
import Ui.Color as Color
import Ui.FocusRing exposing (focusRing)
import Ui.Modal as Modal
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init
        , update = update shared
        , view = view shared
        , subscriptions = subscriptions
        }


type alias Model =
    { modal : Modal.Model ModalKind Msg }


type ModalKind
    = ConfirmDeleteZoneModal Zone
    | AddPlantingModal AddPlantingStep


type AddPlantingStep
    = AddPlantingModalStep1 Zone
    | AddPlantingModalStep2 Zone Crop
    | AddPlantingModalStep3 Zone Crop Variety Int


type Msg
    = AddZone
    | UpdateZone Bool Zone
    | DeleteZone Slug
    | ShowNewPlantingModal Zone
    | ShowConfirmDeleteZoneModal Zone
    | CloseModal
    | ModalMsg Modal.Msg
    | FocusAddPlantingButton Slug
    | FocusDeleteZoneModalButton Slug
    | AdvanceAddPlantingModal AddPlantingStep
    | OnNewPlantingAmountChange String
    | AddPlanting Slug Slug Slug Int (Maybe Time.Posix)
    | NoOp


init : ( Model, Effect Msg )
init =
    ( { modal = Modal.closed
      }
    , Effect.none
    )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update ({ data } as shared) msg model =
    let
        andThen msg_ ( updatedModel, effect ) =
            update shared msg_ updatedModel |> Tuple.mapSecond (List.singleton >> (::) effect >> Effect.batch)
    in
    case msg of
        NoOp ->
            ( model, Effect.none )

        AddZone ->
            ( model, Effect.fromShared Shared.addZone )

        UpdateZone save zone ->
            ( model, Effect.fromShared (Shared.updateZone save zone) )

        DeleteZone slug ->
            ( { model | modal = Modal.closed }, Effect.fromShared (Shared.deleteZone slug) )

        ShowNewPlantingModal zone ->
            ( { model | modal = Modal.open (AddPlantingModal (AddPlantingModalStep1 zone)) (FocusAddPlantingButton zone.slug) }
            , Effect.focus (addPlantingModalCropButtonId 0) (always NoOp)
            )

        ShowConfirmDeleteZoneModal zone ->
            ( { model | modal = Modal.open (ConfirmDeleteZoneModal zone) (FocusDeleteZoneModalButton zone.slug) }
            , Effect.none
            )

        CloseModal ->
            ( { model | modal = Modal.closed }, Effect.none )

        ModalMsg modalMsg ->
            let
                ( updatedModal, cmd ) =
                    Modal.update modalMsg model.modal
            in
            ( { model | modal = updatedModal }, Effect.fromCmd cmd )

        FocusAddPlantingButton zoneSlug ->
            ( model, Effect.focus (addPlantingButtonId zoneSlug) (always NoOp) )

        FocusDeleteZoneModalButton zoneSlug ->
            ( model, Effect.focus (confirmDeleteButtonId zoneSlug) (always NoOp) )

        AdvanceAddPlantingModal step ->
            let
                elToFocus =
                    case step of
                        AddPlantingModalStep1 _ ->
                            addPlantingModalCropButtonId 0

                        AddPlantingModalStep2 _ _ ->
                            addPlantingModalVarietyButtonId 0

                        AddPlantingModalStep3 _ _ _ _ ->
                            addPlantingModalAmountInputId
            in
            ( { model
                | modal = Modal.mapModel (\_ -> AddPlantingModal step) model.modal
              }
            , Effect.focus elToFocus (always NoOp)
            )

        OnNewPlantingAmountChange amountStr ->
            case ( Modal.kind model.modal, String.toInt amountStr ) of
                ( Just (AddPlantingModal (AddPlantingModalStep3 zoneSlug crop variety _)), Just newAmount ) ->
                    ( { model
                        | modal =
                            Modal.mapModel
                                (\_ ->
                                    AddPlantingModal (AddPlantingModalStep3 zoneSlug crop variety newAmount)
                                )
                                model.modal
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        AddPlanting zoneSlug cropSlug varietySlug amount maybePosix ->
            case maybePosix of
                Nothing ->
                    ( model
                    , Effect.getCurrentTime (Just >> AddPlanting zoneSlug cropSlug varietySlug amount)
                    )

                Just now ->
                    let
                        newPlanting =
                            Planting cropSlug varietySlug amount now
                    in
                    data
                        |> RemoteData.map (.zones >> Dict.get (Slug.map identity) zoneSlug)
                        |> RemoteData.toMaybe
                        |> Maybe.andThen identity
                        |> Maybe.map
                            (\zone ->
                                update shared (UpdateZone True { zone | plantings = newPlanting :: zone.plantings }) model
                            )
                        |> Maybe.withDefault ( model, Effect.none )
                        |> andThen CloseModal


subscriptions : Model -> Sub Msg
subscriptions _ =
    Modal.subscriptions |> Sub.map ModalMsg


addPlantingButtonId : Slug -> String
addPlantingButtonId =
    Slug.map ((++) "add-planting-btn--")


confirmDeleteButtonId : Slug -> String
confirmDeleteButtonId =
    Slug.map ((++) "confirm-delete-btn--")


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "What's growing on!?"
    , body =
        case ( shared.data, shared.now ) of
            ( _, Nothing ) ->
                [ Html.p [] [ Html.text "Loading..." ] ]

            ( NotAsked, _ ) ->
                [ Html.p [] [ Html.text "Loading..." ] ]

            ( Loading, _ ) ->
                [ Html.p [] [ Html.text "Loading..." ] ]

            ( Failure err, _ ) ->
                [ Html.p [] [ Html.text <| "Something went wrong..." ++ err ] ]

            ( Success data, Just now ) ->
                [ div
                    [ css
                        [ Css.displayFlex
                        , Css.flexDirection Css.column
                        , Css.alignItems Css.center
                        , Css.color (Css.hex "333")
                        , Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                            [ Css.color (Css.rgba 255 255 255 0.8)
                            ]
                        ]
                    ]
                    [ viewZones data now
                    , Button.view AddZone
                        [ css
                            [ Css.width (Css.pct 100)
                            , Css.padding (Css.em 2)
                            , Css.marginTop (Css.em 3)
                            , Css.maxWidth (Css.px 400)
                            ]
                        ]
                        [ Html.text "Add a new zone" ]
                    ]
                ]
    , modal = Modal.view ModalMsg (viewModal shared) model.modal
    }


viewZones :
    { data
        | crops : Dict Slug Crop
        , varieties : Dict Slug Variety
        , zones : Dict k { slug : Slug, name : String, plantings : List Planting, index : Int }
    }
    -> Time.Posix
    -> Html Msg
viewZones data now =
    Html.ul
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.listStyle Css.none
            , Css.width (Css.pct 100)
            , Css.maxWidth (Css.px 600)
            , Css.padding Css.zero
            , Css.boxSizing Css.borderBox
            ]
        ]
        (List.map (viewZone data now) (Dict.values data.zones |> List.sortBy .index))


viewZone :
    { data
        | crops : Dict Slug Crop
        , varieties : Dict Slug Variety
    }
    -> Time.Posix
    -> Zone
    -> Html Msg
viewZone data now zone =
    let
        capacity =
            100 - List.sum (List.map .amount zone.plantings)
    in
    Html.li
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "1em"
            , Css.padding2 (Css.em 1) Css.zero
            ]
        ]
        [ Html.div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.spaceBetween
                ]
            ]
            [ Html.input
                [ Attrs.value zone.name
                , Attrs.placeholder "Zone name"
                , Slug.map Attrs.id zone.slug
                , css
                    [ Css.backgroundColor Css.inherit
                    , Css.color Css.inherit
                    , Css.fontWeight Css.bold
                    , Css.fontSize (Css.px 14)
                    , Css.border Css.zero
                    , Css.focus [ Css.outline Css.none ]
                    ]
                , onInput (\name -> UpdateZone False { zone | name = name })
                , onBlur (UpdateZone True zone)
                ]
                []
            , Html.button
                [ Attrs.id <| confirmDeleteButtonId zone.slug
                , css
                    [ Css.backgroundColor (Css.rgba 0 0 0 0)
                    , Css.outline Css.none
                    , Css.border Css.zero
                    , focusRing
                    ]
                , onClick (ShowConfirmDeleteZoneModal zone)
                ]
                [ Html.text "🗑" ]
            ]
        , Html.ul
            [ css
                [ Css.paddingLeft Css.zero
                , Css.height (Css.px 90)
                , Css.displayFlex
                ]
            ]
          <|
            List.map (viewPlanting data now) zone.plantings
                ++ (if capacity > 0 then
                        [ Button.view
                            (ShowNewPlantingModal zone)
                            [ Attrs.id <| addPlantingButtonId zone.slug
                            , css
                                [ Css.width (Css.pct (toFloat capacity))
                                , Css.height (Css.px 90)
                                , Css.display Css.inlineBlock
                                , Css.pseudoClass "not(:first-child)"
                                    [ Css.marginLeft (Css.em 1)
                                    , Css.width (Css.calc (Css.pct (toFloat capacity)) Css.minus (Css.em 1))
                                    ]
                                ]
                            ]
                            [ Html.text "➕" ]
                        ]

                    else
                        []
                   )
        ]


viewPlanting :
    { data
        | crops : Dict Slug Crop
        , varieties : Dict Slug Variety
    }
    -> Time.Posix
    -> Planting
    -> Html Msg
viewPlanting { crops, varieties } now planting =
    Maybe.map2
        (\crop variety ->
            div
                [ css
                    [ Css.display Css.inlineBlock
                    , Css.height (Css.px 90)
                    , Css.width (Css.pct <| toFloat planting.amount)
                    , Css.pseudoClass "not(:first-child)"
                        [ Css.marginLeft (Css.em 1)
                        , Css.width (Css.calc (Css.pct <| toFloat planting.amount) Css.minus (Css.em 1))
                        ]
                    ]
                ]
                [ div
                    [ css
                        [ Color.styles crop.color
                        , Css.overflow Css.hidden
                        , Css.padding (Css.px 20)
                        , Css.borderRadius (Css.px 4)
                        , Css.whiteSpace Css.noWrap
                        , Css.hover
                            [ Css.property "min-width" "fit-content"
                            , Css.zIndex (Css.int 1)
                            , Css.position Css.relative
                            , Css.outline3 (Css.em 1) Css.solid (Css.hex "111")
                            ]
                        ]
                    ]
                    [ span
                        [ css
                            [ Css.textTransform Css.uppercase
                            , Css.fontWeight Css.bold
                            , Css.fontSize (Css.px 18)
                            ]
                        ]
                        [ Html.text crop.name ]
                    , span
                        [ css
                            [ Css.fontSize (Css.px 12)
                            , Css.displayFlex
                            , Css.flexDirection Css.column
                            ]
                        ]
                        [ span [] [ Html.text variety.name ]
                        , span [] [ viewDaysAgo now planting.time ]
                        ]
                    ]
                ]
        )
        (Dict.get (Slug.map identity) planting.cropId crops)
        (Dict.get (Slug.map identity) planting.varietyId varieties)
        |> Maybe.withDefault (Html.text "Something went wrong...")


viewDaysAgo : Time.Posix -> Time.Posix -> Html msg
viewDaysAgo now timestamp =
    let
        millisInDay =
            86400000

        daysSince =
            (Time.toMillis Time.utc now - Time.toMillis Time.utc timestamp) // millisInDay
    in
    Html.text (String.fromInt daysSince ++ " days ago")


viewModal : Shared.Model -> ModalKind -> Html Msg
viewModal { data } modalKind =
    case ( modalKind, data ) of
        ( ConfirmDeleteZoneModal zone, _ ) ->
            viewDeleteZoneModal zone

        ( AddPlantingModal step, RemoteData.Success data_ ) ->
            viewAddPlantingModal data_ step

        ( _, RemoteData.NotAsked ) ->
            Html.text "Loading..."

        ( _, RemoteData.Loading ) ->
            Html.text "Loading..."

        ( _, RemoteData.Failure err ) ->
            Html.text <| "Something went wrong..." ++ err


deleteZoneModalConfirmButtonId : String
deleteZoneModalConfirmButtonId =
    "delete-zone-button"


viewDeleteZoneModal : Zone -> Html Msg
viewDeleteZoneModal zone =
    div
        [ css
            [ Css.padding (Css.em 1)
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            ]
        ]
        [ p [] [ Html.text <| "Are you sure you want to delete " ++ zone.name ++ "?" ]
        , div
            [ css
                [ Css.displayFlex
                , Css.property "gap" "1em"
                ]
            ]
            [ Button.view (DeleteZone zone.slug)
                [ Attrs.id deleteZoneModalConfirmButtonId
                , css [ Color.styles Color.Red ]
                ]
                [ Html.text "Delete" ]
            , Button.view CloseModal [] [ Html.text "Cancel" ]
            ]
        ]


addPlantingModalCropButtonId : Int -> String
addPlantingModalCropButtonId index =
    "add-planting-modal-crop-button-" ++ String.fromInt index


addPlantingModalVarietyButtonId : Int -> String
addPlantingModalVarietyButtonId index =
    "add-planting-modal-variety-button-" ++ String.fromInt index


addPlantingModalAmountInputId : String
addPlantingModalAmountInputId =
    "add-planting-modal-amount-input"


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
                |> List.indexedMap
                    (\index crop ->
                        Button.view (AdvanceAddPlantingModal (AddPlantingModalStep2 zoneSlug crop))
                            [ Attrs.id (addPlantingModalCropButtonId index)
                            , css
                                [ Color.styles crop.color
                                , largeButtonStyles
                                ]
                            ]
                            [ Html.text crop.name ]
                    )
                |> wrappingFlexRow

        AddPlantingModalStep2 zoneSlug selectedCrop ->
            List.filterMap (\slug -> Dict.get (Slug.map identity) slug varieties) selectedCrop.varieties
                |> List.indexedMap
                    (\index variety ->
                        Button.view (AdvanceAddPlantingModal (AddPlantingModalStep3 zoneSlug selectedCrop variety 100))
                            [ Attrs.id (addPlantingModalVarietyButtonId index)
                            , css
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
                    [ Attrs.id addPlantingModalAmountInputId
                    , Attrs.type_ "range"
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

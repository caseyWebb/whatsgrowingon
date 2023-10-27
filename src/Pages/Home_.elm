module Pages.Home_ exposing (Model, Msg, page)

import Css
import Css.Media as Media
import Data exposing (..)
import Effect exposing (Effect)
import GenericDict as Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div, span)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events exposing (onBlur, onInput)
import Page
import RemoteData exposing (RemoteData(..))
import Request exposing (Request)
import Shared
import Slug exposing (Slug)
import Time
import Ui.Button as Button
import Ui.Color as Color
import View exposing (View)


type alias Model =
    ()


type Msg
    = AddZone
    | UpdateZone Bool Zone
    | ShowNewPlantingModal Zone


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init
        , update = update
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Effect Msg )
init =
    ( (), Effect.none )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        AddZone ->
            ( model, Effect.fromShared Shared.addZone )

        UpdateZone save zone ->
            ( model, Effect.fromShared (Shared.updateZone save zone) )

        ShowNewPlantingModal zone ->
            ( model, Effect.fromShared <| Shared.showAddPlantingModal zone )


view : Shared.Model -> Model -> View Msg
view shared _ =
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
        [ Html.input
            [ Attrs.value zone.name
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
                            [ css
                                [ Css.width (Css.pct (toFloat capacity))
                                , Css.height (Css.px 90)
                                , Css.display Css.inlineBlock
                                , Css.pseudoClass "not(:first-child)"
                                    [ Css.marginLeft (Css.em 1)
                                    , Css.width (Css.calc (Css.pct (toFloat capacity)) Css.minus (Css.em 1))
                                    ]
                                ]
                            ]
                            [ Html.text "+" ]
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

                            -- , Css.borderRight3 (Css.em 1) Css.solid (Css.hex "111")
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

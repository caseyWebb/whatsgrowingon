module Pages.Home_ exposing (Model, Msg, page)

import Css
import Css.Media as Media
import Data exposing (..)
import Effect exposing (Effect)
import GenericDict as Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div, span)
import Html.Styled.Attributes as Attrs exposing (css)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Page
import RemoteData exposing (RemoteData(..))
import Request exposing (Request)
import Shared
import Slug exposing (Slug)
import Time
import View exposing (View)


type alias Model =
    ()


type Msg
    = AddZone
    | UpdateZone Bool Zone
    | ShowNewPlantingModal Slug


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

        ShowNewPlantingModal zoneSlug ->
            ( model, Effect.fromShared <| Shared.showAddPlantingModal zoneSlug )


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
                        ]
                    ]
                    [ viewZones data now
                    , Html.button
                        [ onClick AddZone
                        , css
                            [ Css.fontWeight Css.bold
                            , Css.fontSize (Css.px 18)
                            , Css.backgroundColor Css.inherit
                            , Css.border3 (Css.px 3) Css.solid (Css.hex "333")
                            , Css.padding2 (Css.em 0.5) (Css.em 1)
                            , Css.outline Css.none
                            , Css.borderRadius (Css.px 4)
                            , Css.color (Css.hex "000")
                            , Css.hover
                                [ Css.borderColor (Css.hex "000")
                                , Css.color (Css.hex "fff")
                                , Css.backgroundColor (Css.hex "111")
                                ]
                            , Css.focus
                                [ Css.borderColor (Css.hex "000")
                                , Css.color (Css.hex "fff")
                                , Css.outline3 (Css.px 5) Css.solid (Css.hex "000")
                                , Css.outlineOffset (Css.px 5)
                                ]
                            , Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                                [ Css.color (Css.hex "fff")
                                , Css.hover
                                    [ Css.borderColor (Css.hex "fff")
                                    , Css.color (Css.hex "fff")
                                    ]
                                , Css.focus
                                    [ Css.borderColor (Css.hex "fff")
                                    , Css.color (Css.hex "fff")
                                    , Css.outlineColor (Css.hex "fff")
                                    ]
                                ]
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
            , Css.property "gap" "1em"
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
    Html.li
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "1em"
            , Css.padding2 (Css.em 1) (Css.em 1)
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
        , div
            [ css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                ]
            ]
            [ Html.ul
                [ css
                    [ Css.displayFlex
                    , Css.flexDirection Css.row
                    , Css.paddingLeft Css.zero
                    , Css.property "gap" "10px"
                    ]
                ]
              <|
                List.map (viewPlanting data now) zone.plantings
                    ++ [ Html.button
                            [ onClick (ShowNewPlantingModal zone.slug)
                            , css
                                [ Css.fontWeight Css.bold
                                , Css.fontSize (Css.px 18)
                                , Css.backgroundColor Css.inherit
                                , Css.flexGrow (Css.int 1)
                                , Css.border3 (Css.px 1) Css.solid (Css.hex "333")
                                , Css.padding2 (Css.em 0.5) (Css.em 1)
                                , Css.outline Css.none
                                , Css.borderRadius (Css.px 4)
                                , Css.color (Css.hex "000")
                                , Css.hover
                                    [ Css.borderColor (Css.hex "000")
                                    , Css.color (Css.hex "fff")
                                    ]
                                , Css.focus
                                    [ Css.borderColor (Css.hex "000")
                                    , Css.color (Css.hex "fff")
                                    , Css.outline3 (Css.px 5) Css.solid (Css.hex "000")
                                    , Css.outlineOffset (Css.px 5)
                                    ]
                                , Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                                    [ Css.color (Css.hex "fff")
                                    , Css.hover
                                        [ Css.borderColor (Css.hex "fff")
                                        , Css.color (Css.hex "fff")
                                        ]
                                    , Css.focus
                                        [ Css.borderColor (Css.hex "fff")
                                        , Css.color (Css.hex "fff")
                                        , Css.outlineColor (Css.hex "fff")
                                        ]
                                    ]
                                ]
                            ]
                            [ Html.text "+" ]
                       ]
            ]
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
                    [ Css.color (Css.hex "fff")
                    , Css.backgroundColor (Css.hex "4c1010")
                    , Css.padding (Css.px 20)
                    , Css.borderRadius (Css.px 4)
                    , Css.boxSizing Css.borderBox
                    , Css.width (Css.calc (Css.pct <| planting.amount * 100) Css.minus (Css.px 5))
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

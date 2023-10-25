module Shared exposing
    ( Model
    , Msg
    , ToBackend(..)
    , ToFrontend(..)
    , addZone
    , fromBackend
    , init
    , showModal
    , subscriptions
    , update
    , updateZone
    , view
    )

import Css
import Css.Global
import Css.Media
import Data exposing (..)
import GenericDict as Dict exposing (Dict)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attrs
import Modal exposing (Modal)
import Random
import RemoteData exposing (RemoteData)
import Request exposing (Request)
import Slug exposing (Slug)
import View exposing (View)


addZone : Msg
addZone =
    AddZone


updateZone : Bool -> Zone -> Msg
updateZone =
    UpdateZone


showModal : Modal -> Msg
showModal =
    ShowModal


fromBackend : ToFrontend -> Msg
fromBackend =
    FromBackend



-- INIT


type alias Model =
    { zones : RemoteData String (Dict Slug Zone)
    , modal : Maybe Modal
    }


init : { toBackend : ToBackend -> Cmd msg } -> Request -> ( Model, Cmd msg )
init { toBackend } _ =
    ( { zones = RemoteData.Loading
      , modal = Nothing
      }
    , toBackend <| FetchZones
    )



-- UPDATE


type Msg
    = FromBackend ToFrontend
    | AddZone
    | UpdateZone Bool Zone
    | GotNewSlug Slug
    | ShowModal Modal
    | CloseModal


type ToBackend
    = FetchZones
    | SaveZone Zone


type ToFrontend
    = GotZones (Dict Slug Zone)


update : { toBackend : ToBackend -> Cmd Msg } -> Request -> Msg -> Model -> ( Model, Cmd Msg )
update { toBackend } _ msg model =
    case msg of
        FromBackend (GotZones zones) ->
            ( { model | zones = RemoteData.Success zones }
            , Cmd.none
            )

        AddZone ->
            ( model
            , Random.generate GotNewSlug Slug.random
            )

        GotNewSlug slug ->
            let
                zone =
                    Slug.map
                        (\str ->
                            { slug = slug
                            , name = str
                            , plantings = []
                            }
                        )
                        slug
            in
            ( { model
                | zones =
                    RemoteData.map (Dict.insert (Slug.map identity) slug zone) model.zones
              }
            , toBackend (SaveZone zone)
            )

        UpdateZone save zone ->
            ( { model
                | zones =
                    RemoteData.map (Dict.insert (Slug.map identity) zone.slug zone) model.zones
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


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none



-- VIEW


view :
    Request
    -> { page : View msg, toMsg : Msg -> msg }
    -> Model
    -> View msg
view req { page, toMsg } model =
    let
        hasModal =
            Maybe.map (always True) model.modal |> Maybe.withDefault False
    in
    { title =
        page.title
    , body =
        [ Html.div
            (Attrs.css
                [ Css.width (Css.pct 100)
                , Css.minHeight (Css.vh 100)
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.alignItems Css.center
                , Css.fontFamily Css.sansSerif
                , Css.textTransform Css.lowercase
                , Css.Media.withMediaQuery [ "(prefers-color-scheme: dark)" ]
                    [ Css.backgroundColor (Css.hex "111")
                    , Css.color (Css.hex "ccc")
                    ]
                ]
                :: (if hasModal then
                        [ Attrs.attribute "inert" "true" ]

                    else
                        []
                   )
            )
            page.body
        , Maybe.map (Modal.view { closeModal = toMsg CloseModal }) model.modal
            |> Maybe.withDefault (Html.text "")
        ]
    }

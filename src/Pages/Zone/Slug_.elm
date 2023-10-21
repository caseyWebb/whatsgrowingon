module Pages.Zone.Slug_ exposing (Model, Msg, page)

import Data exposing (Zone)
import Gen.Params.Zone.Slug_ exposing (Params)
import GenericDict as Dict
import Html
import Page
import Request
import Shared
import Slug exposing (Slug)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    let
        slug =
            Slug.fromString req.params.slug
    in
    Page.element
        { init = init shared slug
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }


type Model
    = Loading
    | Ready Data


type alias Data =
    { zone : Zone
    }


init : Shared.Model -> Slug -> ( Model, Cmd Msg )
init shared slug =
    if Slug.isNew slug then
        ( Ready { zone = { slug = slug, name = "", plantings = [] } }, Cmd.none )

    else
        case Dict.get (Slug.map identity) slug shared.zones of
            Just zone ->
                ( Ready { zone = zone }, Cmd.none )

            Nothing ->
                ( Loading, Cmd.none )


type Msg
    = NoOp


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update shared msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Shared.Model -> Model -> View Msg
view shared model =
    case model of
        Loading ->
            viewLoading

        Ready data_ ->
            viewZone data_.zone


viewLoading : View Msg
viewLoading =
    { title = "Loading"
    , body =
        [ Html.h1 [] [ Html.text "Loading" ]
        ]
    }


viewNewZone : View Msg
viewNewZone =
    { title = "New Zone"
    , body =
        [ Html.h1 [] [ Html.text "Create a new planting zone" ]
        ]
    }


viewZone : Zone -> View Msg
viewZone zone =
    { title = "Zone"
    , body =
        [ Html.h1 [] [ Html.text "Zone" ]
        ]
    }
